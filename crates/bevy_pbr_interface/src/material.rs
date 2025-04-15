use bevy_asset::Asset;

pub trait Material: Asset + Clone + Sized {}
