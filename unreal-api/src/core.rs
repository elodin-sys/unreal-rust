use bevy::app::{App, First, Plugin, PluginGroup, PostUpdate, PreUpdate, ScheduleRunnerPlugin};
use bevy::transform::components::Transform;
use bevy::MinimalPlugins;
use bevy_ecs::schedule::ScheduleLabel;
use bevy_ecs::system::EntityCommands;
use bevy_ecs::{prelude::*, system::Command};
use ffi::{ActorClass, ActorComponentPtr, ActorComponentType, Quaternion, UEventType};
use std::collections::HashSet;
use std::ffi::c_void;

use crate::{
    api::UnrealApi,
    ffi::{self, AActorOpaque},
    input::Input,
    math::{Quat, Vec3},
    module::{bindings, UserModule},
    physics::PhysicsComponent,
};

pub struct UnrealCore {
    app: App,
}

pub struct CorePlugin;

impl Plugin for CorePlugin {
    fn build(&self, app: &mut App) {
        let mut registry = app
            .world
            .get_resource_or_insert_with(ReflectionRegistry::default);
        registry.register::<UnrealTransform>();
        registry.register::<ActorComponent>();
        registry.register::<PlayerInputComponent>();
        registry.register::<ParentComponent>();
        registry.register::<PhysicsComponent>();

        app.insert_resource(Frame::default())
            .insert_resource(Time::default())
            .insert_resource(Input::default())
            .insert_resource(UnrealApi::default())
            .insert_resource(SpawnedActors::default())
            // TODO: Order matters here. Needs to be defined after the stages
            .add_event::<OnActorBeginOverlapEvent>()
            .add_event::<OnActorEndOverlapEvent>()
            .add_event::<ActorHitEvent>()
            .add_event::<ActorSpawnedEvent>()
            .add_event::<ActorDestroyEvent>()
            .add_systems(
                First,
                (
                    process_bevy_spawned_actor,
                    process_actor_spawned.after(process_bevy_spawned_actor),
                    process_actor_destroyed,
                ),
            )
            .add_systems(
                PreUpdate,
                (
                    update_input,
                    download_transform_from_unreal,
                    download_physics_from_unreal,
                ),
            )
            .add_systems(
                PostUpdate,
                (upload_transform_to_unreal, upload_physics_to_unreal),
            );
    }
}

impl UnrealCore {
    pub fn new(user_module: &dyn UserModule) -> Self {
        let mut app = App::new();
        app.insert_resource(ReflectionRegistry::default())
            .add_plugins((
                MinimalPlugins.set(ScheduleRunnerPlugin::run_once()),
                CorePlugin,
            ));
        user_module.init(&mut app);
        Self { app }
    }

    pub fn begin_play(&mut self, user_module: &dyn UserModule) {
        *self = Self::new(user_module);
        self.app.update();
    }
    pub fn tick(&mut self, dt: f32) {
        if let Some(mut frame) = self.app.world.get_resource_mut::<Frame>() {
            frame.dt = dt;
        }
        if let Some(mut time) = self.app.world.get_resource_mut::<Time>() {
            time.time += dt as f64;
        }
        self.app.update();
        self.app.world.clear_trackers();
    }
}

pub unsafe extern "C" fn retrieve_uuids(ptr: *mut ffi::Uuid, len: *mut usize) {
    if let Some(global) = crate::module::MODULE.as_mut() {
        let reflection_registry = global
            .core
            .app
            .world
            .get_resource::<ReflectionRegistry>()
            .expect("reflection registry not found");
        if ptr.is_null() {
            *len = reflection_registry.uuid_set.len();
        } else {
            let slice = std::ptr::slice_from_raw_parts_mut(ptr, *len);
            for (idx, uuid) in reflection_registry.uuid_set.iter().take(*len).enumerate() {
                (*slice)[idx] = to_ffi_uuid(*uuid);
            }
        }
    }
}

#[derive(Event)]
pub struct ActorSpawnedEvent {
    pub actor: ActorPtr,
}

#[derive(Event)]
pub struct OnActorBeginOverlapEvent {
    pub overlapped_actor: ActorPtr,
    pub other: ActorPtr,
}

#[derive(Event)]
pub struct OnActorEndOverlapEvent {
    pub overlapped_actor: ActorPtr,
    pub other: ActorPtr,
}

#[derive(Event)]
pub struct ActorHitEvent {
    pub self_actor: ActorPtr,
    pub other: ActorPtr,
    pub normal_impulse: Vec3,
}

#[derive(Event)]
pub struct ActorDestroyEvent {
    pub actor: ActorPtr,
}

pub unsafe extern "C" fn unreal_event(ty: *const UEventType, data: *const c_void) {
    if let Some(global) = crate::module::MODULE.as_mut() {
        match *ty {
            UEventType::ActorSpawned => {
                let actor_spawned_event = data as *const ffi::ActorSpawnedEvent;
                global.core.app.world.send_event(ActorSpawnedEvent {
                    actor: ActorPtr((*actor_spawned_event).actor),
                });
            }
            UEventType::ActorBeginOverlap => {
                let overlap = data as *const ffi::ActorBeginOverlap;
                global.core.app.world.send_event(OnActorBeginOverlapEvent {
                    overlapped_actor: ActorPtr((*overlap).overlapped_actor),
                    other: ActorPtr((*overlap).other),
                });
            }
            UEventType::ActorEndOverlap => {
                let overlap = data as *const ffi::ActorEndOverlap;
                global.core.app.world.send_event(OnActorEndOverlapEvent {
                    overlapped_actor: ActorPtr((*overlap).overlapped_actor),
                    other: ActorPtr((*overlap).other),
                });
            }
            UEventType::ActorOnHit => {
                let hit = data as *const ffi::ActorHitEvent;
                global.core.app.world.send_event(ActorHitEvent {
                    self_actor: ActorPtr((*hit).self_actor),
                    other: ActorPtr((*hit).other),
                    normal_impulse: (*hit).normal_impulse.into(),
                });
            }
            UEventType::ActorDestroy => {
                let destroy = data as *const ffi::ActorDestroyEvent;
                global.core.app.world.send_event(ActorDestroyEvent {
                    actor: ActorPtr((*destroy).actor),
                });
            }
        }
    }
}
extern "C" fn get_field_float_value(
    uuid: ffi::Uuid,
    entity: ffi::Entity,
    idx: u32,
    out: *mut f32,
) -> u32 {
    let result = std::panic::catch_unwind(|| {
        if let Some(ReflectValue::Float(f)) = get_field_value(uuid, entity, idx) {
            unsafe {
                *out = f;
            }
            1
        } else {
            0
        }
    });
    result.unwrap_or(0)
}
extern "C" fn get_field_quat_value(
    uuid: ffi::Uuid,
    entity: ffi::Entity,
    idx: u32,
    out: *mut Quaternion,
) -> u32 {
    let result = std::panic::catch_unwind(|| {
        if let Some(ReflectValue::Quat(q)) = get_field_value(uuid, entity, idx) {
            unsafe {
                *out = q.into();
            }
            1
        } else {
            0
        }
    });
    result.unwrap_or(0)
}

extern "C" fn get_field_vector3_value(
    uuid: ffi::Uuid,
    entity: ffi::Entity,
    idx: u32,
    out: *mut ffi::Vector3,
) -> u32 {
    let result = std::panic::catch_unwind(|| {
        if let Some(ReflectValue::Vector3(v)) = get_field_value(uuid, entity, idx) {
            unsafe {
                *out = v.into();
            }
            1
        } else {
            0
        }
    });
    result.unwrap_or(0)
}
extern "C" fn get_field_bool_value(
    uuid: ffi::Uuid,
    entity: ffi::Entity,
    idx: u32,
    out: *mut u32,
) -> u32 {
    let result = std::panic::catch_unwind(|| {
        if let Some(ReflectValue::Bool(b)) = get_field_value(uuid, entity, idx) {
            unsafe {
                *out = b as u32;
            }
            1
        } else {
            0
        }
    });
    result.unwrap_or(0)
}

fn get_field_value(uuid: ffi::Uuid, entity: ffi::Entity, idx: u32) -> Option<ReflectValue> {
    let uuid = from_ffi_uuid(uuid);
    unsafe {
        let global = crate::module::MODULE.as_mut()?;
        let reflection_registry = global
            .core
            .app
            .world
            .get_resource::<ReflectionRegistry>()
            .expect("reflection registry not found");

        let reflect = reflection_registry.reflect.get(&uuid)?;

        let entity = Entity::try_from_bits(entity.id).ok()?;
        reflect.get_field_value(&global.core.app.world, entity, idx)
    }
}

unsafe extern "C" fn number_of_fields(uuid: ffi::Uuid, out: *mut u32) -> u32 {
    fn get_number_fields(uuid: ffi::Uuid) -> Option<u32> {
        let global = unsafe { crate::module::MODULE.as_mut() }?;
        let uuid = from_ffi_uuid(uuid);
        let reflection_registry = global
            .core
            .app
            .world
            .get_resource::<ReflectionRegistry>()
            .expect("reflection registry not found");

        let reflect = reflection_registry.reflect.get(&uuid)?;
        Some(reflect.number_of_fields() as u32)
    }
    let result = std::panic::catch_unwind(|| {
        if let Some(count) = get_number_fields(uuid) {
            *out = count;
            1
        } else {
            0
        }
    });
    result.unwrap_or(0)
}
unsafe extern "C" fn get_type_name(uuid: ffi::Uuid, out: *mut ffi::Utf8Str) -> u32 {
    fn get_type_name(uuid: ffi::Uuid) -> Option<&'static str> {
        let global = unsafe { crate::module::MODULE.as_mut() }?;
        let uuid = from_ffi_uuid(uuid);
        let reflection_registry = global
            .core
            .app
            .world
            .get_resource::<ReflectionRegistry>()
            .expect("reflection registry not found");

        let reflect = reflection_registry.reflect.get(&uuid)?;
        Some(reflect.name())
    }
    let result = std::panic::catch_unwind(|| {
        if let Some(name) = get_type_name(uuid) {
            *out = ffi::Utf8Str::from(name);
            1
        } else {
            0
        }
    });
    result.unwrap_or(0)
}
unsafe extern "C" fn has_component(entity: ffi::Entity, uuid: ffi::Uuid) -> u32 {
    fn has_component(entity: ffi::Entity, uuid: ffi::Uuid) -> Option<u32> {
        let global = unsafe { crate::module::MODULE.as_ref() }?;
        let uuid = from_ffi_uuid(uuid);
        let reflection_registry = global
            .core
            .app
            .world
            .get_resource::<ReflectionRegistry>()
            .expect("reflection registry not found");

        let reflect = reflection_registry.reflect.get(&uuid)?;
        let entity = Entity::try_from_bits(entity.id).ok()?;
        Some(reflect.has_component(&global.core.app.world, entity) as u32)
    }
    let result = std::panic::catch_unwind(|| has_component(entity, uuid).unwrap_or(0));
    result.unwrap_or(0)
}

unsafe extern "C" fn is_editor_component(uuid: ffi::Uuid) -> u32 {
    fn is_editor_component_inner(uuid: ffi::Uuid) -> Option<u32> {
        let global = unsafe { crate::module::MODULE.as_mut() }?;
        let uuid = from_ffi_uuid(uuid);
        let reflection_registry = global
            .core
            .app
            .world
            .get_resource::<ReflectionRegistry>()
            .expect("reflection registry not found");

        Some(
            if reflection_registry
                .insert_editor_component
                .contains_key(&uuid)
            {
                1
            } else {
                0
            },
        )
    }
    let result = std::panic::catch_unwind(|| is_editor_component_inner(uuid).unwrap_or(0));
    result.unwrap_or(0)
}

unsafe extern "C" fn get_field_name(uuid: ffi::Uuid, idx: u32, out: *mut ffi::Utf8Str) -> u32 {
    fn get_field_name(uuid: ffi::Uuid, idx: u32) -> Option<&'static str> {
        let global = unsafe { crate::module::MODULE.as_mut() }?;
        let uuid = from_ffi_uuid(uuid);
        let reflection_registry = global
            .core
            .app
            .world
            .get_resource::<ReflectionRegistry>()
            .expect("reflection registry not found");

        let reflect = reflection_registry.reflect.get(&uuid)?;
        reflect.get_field_name(idx)
    }
    let result = std::panic::catch_unwind(|| {
        if let Some(name) = get_field_name(uuid, idx) {
            *out = ffi::Utf8Str::from(name);
            1
        } else {
            0
        }
    });
    result.unwrap_or(0)
}
unsafe extern "C" fn get_field_type(
    uuid: ffi::Uuid,
    idx: u32,
    out: *mut ffi::ReflectionType,
) -> u32 {
    fn get_field_type(uuid: ffi::Uuid, idx: u32) -> Option<ffi::ReflectionType> {
        let global = unsafe { crate::module::MODULE.as_mut() }?;
        let uuid = from_ffi_uuid(uuid);
        let reflection_registry = global
            .core
            .app
            .world
            .get_resource::<ReflectionRegistry>()
            .expect("reflection registry not found");

        let reflect = reflection_registry.reflect.get(&uuid)?;
        let ty = reflect.get_field_type(idx)?;
        Some(match ty {
            ReflectType::Bool => ffi::ReflectionType::Bool,
            ReflectType::Float => ffi::ReflectionType::Float,
            ReflectType::Vector3 => ffi::ReflectionType::Vector3,
            ReflectType::Quat => ffi::ReflectionType::Quaternion,
            ReflectType::UClass => ffi::ReflectionType::UClass,
            ReflectType::USound => ffi::ReflectionType::USound,
            ReflectType::Composite => ffi::ReflectionType::Composite,
        })
    }
    let result = std::panic::catch_unwind(|| {
        if let Some(ty) = get_field_type(uuid, idx) {
            *out = ty;
            1
        } else {
            0
        }
    });
    result.unwrap_or(0)
}

pub fn from_ffi_uuid(uuid: ffi::Uuid) -> Uuid {
    unsafe {
        let arr: [u32; 4] = [uuid.a, uuid.b, uuid.c, uuid.d];
        Uuid::from_bytes(std::mem::transmute(arr))
    }
}
pub fn to_ffi_uuid(uuid: Uuid) -> ffi::Uuid {
    unsafe {
        let [a, b, c, d]: [u32; 4] = std::mem::transmute(*uuid.as_bytes());
        ffi::Uuid { a, b, c, d }
    }
}

pub fn create_reflection_fns() -> ffi::ReflectionFns {
    ffi::ReflectionFns {
        is_editor_component,
        has_component,
        get_field_bool_value,
        get_field_float_value,
        get_field_quat_value,
        get_field_vector3_value,
        number_of_fields,
        get_field_name,
        get_field_type,
        get_type_name,
    }
}

unsafe extern "C" fn allocate(size: usize, align: usize, ptr: *mut ffi::RustAlloc) -> u32 {
    use std::alloc::{alloc, Layout};
    let layout = Layout::from_size_align(size, align);
    match layout {
        Ok(layout) => {
            let alloc_ptr = alloc(layout);
            *ptr = ffi::RustAlloc {
                ptr: alloc_ptr,
                size,
                align,
            };

            1
        }
        Err(_) => 0,
    }
}
pub fn create_allocate_fns() -> ffi::AllocateFns {
    ffi::AllocateFns { allocate }
}

pub extern "C" fn tick(dt: f32) -> crate::ffi::ResultCode {
    let r = std::panic::catch_unwind(|| unsafe {
        UnrealCore::tick(&mut crate::module::MODULE.as_mut().unwrap().core, dt);
    });
    match r {
        Ok(_) => ffi::ResultCode::Success,
        Err(_) => ffi::ResultCode::Panic,
    }
}

pub extern "C" fn begin_play() -> ffi::ResultCode {
    let r = std::panic::catch_unwind(|| unsafe {
        let global = crate::module::MODULE.as_mut().unwrap();
        UnrealCore::begin_play(&mut global.core, global.module.as_ref());
    });
    match r {
        Ok(_) => ffi::ResultCode::Success,
        Err(_) => ffi::ResultCode::Panic,
    }
}

pub fn register_core_components(registry: &mut ReflectionRegistry) {
    registry.register::<UnrealTransform>();
    registry.register::<ActorComponent>();
    registry.register::<PlayerInputComponent>();
    registry.register::<ParentComponent>();
    registry.register::<PhysicsComponent>();
}

use unreal_api::{module::ReflectionRegistry, Component};
use unreal_reflect::{
    registry::{ReflectType, ReflectValue},
    Uuid,
};
#[derive(Debug, Hash, PartialEq, Eq, Clone, ScheduleLabel)]
pub struct StartupStage;

// #[derive(Debug, Hash, PartialEq, Eq, Clone, ScheduleLabel)]
// pub enum CoreStage {
//     Startup,
//     RegisterEvent,
//     PreUpdate,
//     Update,
//     PostUpdate,
// }
#[derive(Default, Debug, Copy, Clone, Resource)]
pub struct Frame {
    pub dt: f32,
}

#[derive(Default, Debug, Copy, Clone, Resource)]
pub struct Time {
    pub time: f64,
}

#[derive(Default, Debug, Component)]
#[uuid = "5ad05c2b-7cbc-4081-8819-1997b3e13331"]
pub struct ActorComponent {
    #[reflect(skip)]
    pub actor: ActorPtr,
}

impl ActorComponent {
    pub fn register_on_hit(&mut self) {
        unsafe {
            (bindings().actor_fns.register_actor_on_hit)(self.actor.0);
        }
    }

    pub fn set_owner(&mut self, new_owner: Option<&Self>) {
        unsafe {
            let ptr = new_owner
                .map(|comp| comp.actor.0 as *const AActorOpaque)
                .unwrap_or(std::ptr::null());
            (bindings().actor_fns.set_owner)(self.actor.0, ptr);
        }
    }

    pub fn get_actor_name(&self) -> String {
        unsafe {
            let mut alloc = ffi::RustAlloc::empty();
            (bindings().actor_fns.get_actor_name)(self.actor.0, &mut alloc);
            let name = {
                let slice = std::slice::from_raw_parts(alloc.ptr, alloc.size);
                let name = std::str::from_utf8(slice).unwrap();
                name.to_string()
            };
            alloc.free();
            name
        }
    }
}

#[derive(Default, Debug, Component, Clone)]
#[uuid = "b8738d9e-ab21-47db-8587-4019b38e35a6"]
pub struct UnrealTransform {
    pub position: Vec3,
    pub rotation: Quat,
    pub scale: Vec3,
}

impl From<Transform> for UnrealTransform {
    fn from(value: Transform) -> Self {
        Self {
            position: value.translation,
            rotation: value.rotation,
            scale: value.scale,
        }
    }
}

impl From<UnrealTransform> for Transform {
    fn from(value: UnrealTransform) -> Self {
        Self {
            translation: value.position,
            rotation: value.rotation,
            scale: value.scale,
        }
    }
}

impl UnrealTransform {
    pub fn is_nan(&self) -> bool {
        self.position.is_nan() || self.rotation.is_nan() || self.scale.is_nan()
    }
}

#[derive(Debug, Component)]
#[uuid = "f1e22f5b-2bfe-4ce5-938b-7c093def708e"]
pub struct ParentComponent {
    #[reflect(skip)]
    pub parent: Entity,
}

impl Default for ParentComponent {
    fn default() -> Self {
        todo!()
    }
}

#[derive(Default, Debug, Component)]
#[uuid = "35256309-43b4-4459-9884-eb6e9137faf5"]
pub struct PlayerInputComponent {
    pub direction: Vec3,
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct ActorPtr(pub *mut AActorOpaque);
impl ActorPtr {
    pub fn get_actor_name(&self) -> String {
        unsafe {
            let mut alloc = ffi::RustAlloc::empty();
            (bindings().actor_fns.get_actor_name)(self.0, &mut alloc);
            let name = {
                let slice = std::slice::from_raw_parts(alloc.ptr, alloc.size);
                let name = std::str::from_utf8(slice).unwrap();
                name.to_string()
            };
            alloc.free();
            name
        }
    }

    pub fn set_view_target(&self) {
        unsafe {
            (bindings().actor_fns.set_view_target)(self.0);
        }
    }
}
unsafe impl Send for ActorPtr {}
unsafe impl Sync for ActorPtr {}
impl Default for ActorPtr {
    fn default() -> Self {
        Self(std::ptr::null_mut())
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnrealPtr<T> {
    pub ptr: *mut c_void,
    _m: std::marker::PhantomData<T>,
}
impl<T> UnrealPtr<T> {
    pub fn from_raw(ptr: *mut c_void) -> Self {
        Self {
            ptr,
            ..Default::default()
        }
    }
}
unsafe impl<T> Send for UnrealPtr<T> {}
unsafe impl<T> Sync for UnrealPtr<T> {}
impl<T> Default for UnrealPtr<T> {
    fn default() -> Self {
        Self {
            ptr: std::ptr::null_mut(),
            _m: Default::default(),
        }
    }
}
impl<T> Copy for UnrealPtr<T> {}

impl<T> Clone for UnrealPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Debug)]
pub enum Capsule {}
#[derive(Debug)]
pub enum Primitive {}

fn download_physics_from_unreal(mut query: Query<&mut PhysicsComponent>) {
    for mut physics in query.iter_mut() {
        physics.download_state();
    }
}
fn upload_physics_to_unreal(mut query: Query<&mut PhysicsComponent>) {
    for mut physics in query.iter_mut() {
        physics.download_state();
    }
}
fn download_transform_from_unreal(
    mut query: Query<(&ActorComponent, &mut UnrealTransform, &mut Transform)>,
) {
    for (actor, mut unreal_transform, mut transform) in query.iter_mut() {
        let mut position = ffi::Vector3::default();
        let mut rotation = ffi::Quaternion::default();
        let mut scale = ffi::Vector3::default();

        (bindings().actor_fns.get_spatial_data)(
            actor.actor.0,
            &mut position,
            &mut rotation,
            &mut scale,
        );

        unreal_transform.position = position.into();
        unreal_transform.rotation = rotation.into();
        unreal_transform.scale = scale.into();
        *transform = unreal_transform.clone().into();
        assert!(!unreal_transform.is_nan());
    }
}

fn upload_transform_to_unreal(
    mut query: Query<(&ActorComponent, &Transform, &mut UnrealTransform)>,
) {
    for (actor, transform, mut unreal_transform) in query.iter_mut() {
        *unreal_transform = transform.clone().into();
        let is_moveable = unsafe { (bindings().actor_fns.is_moveable)(actor.actor.0) } > 0;
        if !is_moveable {
            continue;
        }
        assert!(!unreal_transform.is_nan());
        (bindings().actor_fns.set_spatial_data)(
            actor.actor.0,
            unreal_transform.position.into(),
            unreal_transform.rotation.into(),
            unreal_transform.scale.into(),
        );
    }
}

fn update_input(mut input: ResMut<Input>) {
    input.update();
}
#[derive(Debug)]
pub struct Despawn {
    pub entity: Entity,
}

impl Command for Despawn {
    fn apply(self, world: &mut World) {
        world.despawn(self.entity);
        if let Some(mut api) = world.get_resource_mut::<UnrealApi>() {
            // If this entity had an actor, we will also remove it from the map. Otherwise
            // `actor_to_entity` will grow indefinitely
            if let Some(actor) = api.entity_to_actor.remove(&self.entity) {
                api.actor_to_entity.remove(&actor);
                unsafe {
                    (bindings().actor_fns.destroy_actor)(actor.0);
                }
            }
        }
    }
}

pub trait UnrealCommandExt {
    fn spawn_actor(
        &mut self,
        class: ActorClass,
        transform: Transform,
    ) -> (EntityCommands<'_>, ActorPtr);
}

impl<'w, 's> UnrealCommandExt for Commands<'w, 's> {
    fn spawn_actor(
        &mut self,
        class: ActorClass,
        transform: Transform,
    ) -> (EntityCommands<'_>, ActorPtr) {
        let mut entity_cmds = self.spawn_empty();
        let actor = entity_cmds.insert_actor(class, transform);
        (entity_cmds, actor)
    }
}

pub trait UnrealEntityCommandsExt {
    fn insert_actor(&mut self, class: ActorClass, transform: Transform) -> ActorPtr;
}

impl UnrealEntityCommandsExt for EntityCommands<'_> {
    fn insert_actor(&mut self, class: ActorClass, transform: Transform) -> ActorPtr {
        let actor = unsafe {
            (bindings().spawn_actor)(
                class,
                transform.translation.into(),
                transform.rotation.into(),
                transform.scale.into(),
            )
        };
        self.insert((
            transform,
            UnrealTransform::from(transform),
            ActorComponent {
                actor: ActorPtr(actor),
            },
        ));
        ActorPtr(actor)
    }
}

/// It can can that actors are destroyed inside unreal for example from the kill plane. We need to
/// make sure to unregister them, otherwise we will end up with a dangling pointer in Rust.
/// Here we actually despawn the whole entity instead of just removing the `ActorComponent` because
/// it would be strange to keep the rust entity part alive, if the actor has been removed.
fn process_actor_destroyed(
    mut api: ResMut<UnrealApi>,
    mut reader: EventReader<ActorDestroyEvent>,
    mut commands: Commands,
    mut spawned_actors: ResMut<SpawnedActors>,
) {
    for event in reader.read() {
        if let Some(entity) = api.actor_to_entity.remove(&event.actor) {
            commands.add(Despawn { entity });
        }
        spawned_actors.0.remove(&event.actor);
    }
}

#[derive(Resource, Default)]
struct SpawnedActors(HashSet<ActorPtr>);

fn process_actor_spawned(
    mut api: ResMut<UnrealApi>,
    mut reader: EventReader<ActorSpawnedEvent>,
    mut commands: Commands,
    mut spawned_actors: ResMut<SpawnedActors>,
) {
    unsafe {
        if let Some(global) = crate::module::MODULE.as_mut() {
            for &ActorSpawnedEvent { actor } in reader.read() {
                if spawned_actors.0.contains(&actor) {
                    continue;
                }
                let mut entity_cmds = commands.spawn_empty();

                let mut len = 0;
                (bindings().editor_component_fns.get_editor_components)(
                    actor.0,
                    std::ptr::null_mut(),
                    &mut len,
                );

                let mut uuids = vec![ffi::Uuid::default(); len];
                (bindings().editor_component_fns.get_editor_components)(
                    actor.0,
                    uuids.as_mut_ptr(),
                    &mut len,
                );
                // We might have gotten back fewer uuids, so we truncate
                uuids.truncate(len);

                // We register all the components that are on the actor in unreal and add
                // them to the entity
                for uuid in uuids {
                    let uuid = from_ffi_uuid(uuid);
                    let reflection_registry = global
                        .core
                        .app
                        .world
                        .get_resource::<ReflectionRegistry>()
                        .expect("reflection registry not found");
                    if let Some(insert) = reflection_registry.insert_editor_component.get(&uuid) {
                        insert.insert_component(actor.0, uuid, &mut entity_cmds);
                    }
                }

                entity_cmds.insert((ActorComponent { actor }, UnrealTransform::default()));

                // Create a physics component if the root component is a primitive
                // component
                // TODO: We probably should get ALL the primitive components as well
                let mut root_component = ActorComponentPtr::default();
                (bindings().actor_fns.get_root_component)(actor.0, &mut root_component);
                if root_component.ty == ActorComponentType::Primitive
                    && !root_component.ptr.is_null()
                {
                    let physics_component =
                        PhysicsComponent::new(UnrealPtr::from_raw(root_component.ptr));
                    entity_cmds.insert(physics_component);
                }

                let entity = entity_cmds.id();

                api.register_actor(actor, entity);

                // Update the `EntityComponent` with the entity id so we can easily access
                // it in blueprint etc
                (bindings().actor_fns.set_entity_for_actor)(
                    actor.0,
                    ffi::Entity {
                        id: entity.to_bits(),
                    },
                );
                spawned_actors.0.insert(actor);
            }
        }
    }
}

fn process_bevy_spawned_actor(
    query: Query<&ActorComponent, Added<ActorComponent>>,
    mut spawned_actors: ResMut<SpawnedActors>,
) {
    for actor in query.iter() {
        spawned_actors.0.insert(actor.actor);
    }
}
