#![expect(missing_docs, reason = "Not all docs are written yet, see #3492.")]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]
#![forbid(unsafe_code)]
#![doc(
    html_logo_url = "https://bevyengine.org/assets/icon.png",
    html_favicon_url = "https://bevyengine.org/assets/icon.png"
)]

extern crate alloc;

pub mod deferred;
pub mod directional_light;
pub mod light;
pub mod material;
pub mod mesh_material;
pub mod pbr_material;
pub mod point_light;
pub mod skin;
pub mod spot_light;

pub use deferred::*;
pub use directional_light::*;
pub use light::*;
pub use material::*;
pub use mesh_material::*;
pub use pbr_material::*;
pub use point_light::*;
pub use skin::*;
pub use spot_light::*;
