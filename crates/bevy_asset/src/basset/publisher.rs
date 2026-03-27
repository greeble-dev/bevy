use crate::{basset::BassetShared, AssetRef};
use alloc::{boxed::Box, vec::Vec};

struct InputManifest {
    paths: Vec<AssetRef<'static>>,
}

struct StagedAsset {
    path: AssetRef<'static>,
    bytes: Box<[u8]>,
}

fn publish(_input_manifest: InputManifest, _shared: &BassetShared) {
    /*
    let mut staged_assets = HashMap::<AssetPath<'static>, StagedAssets>::new();

    let mut input_stack = input_manifest.paths.clone(); // XXX TODO: Avoid clone?

    while Some(input_asset) = input_stack.pop() {
        if action
            try get dependencies and action key from cache
            else load

            push dependencies to stack if not already in staged_assets
            add to staged
        if regular
            try to get dependencies from cache
            else load
            add to staged
    }

    if compression enabled then compress files now
        TODO: Maybe bake this into standalone files? So they can individually
        enable compression? although that means action cache has to use the
        same compression settings if we want to simply copy

        TODO: Do this while staging?
    }
    */
}
