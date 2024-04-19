use std::collections::{HashMap, HashSet};

use bevy::app::App;
use bevy_ecs::{prelude::System, system::Resource};
use unreal_reflect::{registry::ReflectDyn, uuid, TypeUuid};

use crate::{core::UnrealCore, editor_component::InsertEditorComponent, ffi::UnrealBindings};

pub static mut MODULE: Option<Global> = None;
pub struct Global {
    pub core: UnrealCore,
    pub module: Box<dyn UserModule>,
}

pub trait InitUserModule {
    fn initialize() -> Self;
}

pub type EmptySystem = &'static dyn System<In = (), Out = ()>;

pub trait InsertReflectionStruct {
    fn insert(registry: &mut ReflectionRegistry);
}

#[derive(Default, Resource)]
pub struct ReflectionRegistry {
    pub uuid_set: HashSet<uuid::Uuid>,
    pub reflect: HashMap<uuid::Uuid, Box<dyn ReflectDyn + Send + Sync>>,
    pub insert_editor_component: HashMap<uuid::Uuid, Box<dyn InsertEditorComponent + Send + Sync>>,
}

impl ReflectionRegistry {
    pub fn register<T>(&mut self)
    where
        T: InsertReflectionStruct + TypeUuid + 'static,
    {
        if self.uuid_set.contains(&T::TYPE_UUID) {
            panic!(
                "Duplicated UUID {} for {}",
                T::TYPE_UUID,
                std::any::type_name::<T>()
            );
        }
        T::insert(self);
        self.uuid_set.insert(T::TYPE_UUID);
    }
}

pub trait UserModule {
    fn initialize(&self, module: &mut App);
}
pub static mut BINDINGS: Option<UnrealBindings> = None;

#[macro_export]
macro_rules! implement_unreal_module {
    ($module: ty) => {
        #[no_mangle]
        pub unsafe extern "C" fn register_unreal_bindings(
            bindings: $crate::ffi::UnrealBindings,
            rust_bindings: *mut $crate::ffi::RustBindings,
        ) -> u32 {
            std::panic::set_hook(Box::new(|panic_info| {
                let info = panic_info
                    .payload()
                    .downcast_ref::<&'static str>()
                    .copied()
                    .or(panic_info
                        .payload()
                        .downcast_ref::<String>()
                        .map(String::as_str));

                if let Some(s) = info {
                    let location = panic_info.location().map_or("".to_string(), |loc| {
                        format!("{}, at line {}", loc.file(), loc.line())
                    });
                    log::error!("Panic: {} => {}", location, s);
                } else {
                    log::error!("panic occurred");
                }
            }));
            $crate::module::BINDINGS = Some(bindings);
            let _ = $crate::log::init();

            let r = std::panic::catch_unwind(|| unsafe {
                let module = Box::new(<$module as $crate::module::InitUserModule>::initialize());
                let core = $crate::core::UnrealCore::new(module.as_ref());

                $crate::module::MODULE = Some($crate::module::Global { core, module });
                $crate::ffi::RustBindings {
                    retrieve_uuids: $crate::core::retrieve_uuids,
                    tick: $crate::core::tick,
                    begin_play: $crate::core::begin_play,
                    unreal_event: $crate::core::unreal_event,
                    reflection_fns: $crate::core::create_reflection_fns(),
                    allocate_fns: $crate::core::create_allocate_fns(),
                }
            });
            match r {
                Ok(bindings) => {
                    *rust_bindings = bindings;
                    1
                }
                Err(_) => 0,
            }
        }
    };
}

pub fn bindings() -> &'static UnrealBindings {
    unsafe { BINDINGS.as_ref().unwrap() }
}
