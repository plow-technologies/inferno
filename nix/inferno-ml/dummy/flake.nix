# This is for building the `inferno-ml-server` images. The `inferno` flake
# takes an `image-config` input that must contain a `nixosModules.default`
# that will be applied to the image configuration. We need a default input,
# so it is set to `path:...` pointing to this file
#
# If no extra configuration is required, the input can remain as-is. However,
# it can be overridden with a different `nixosModules` by using `follows`
# (in flakes where `inferno` is taken as an input) or by using `--override-input`
# from the command line
#
# In addition, the `image-config` input can define a `nixosModules.postgresql`
# which will be applied to the Postgres images. This is not necessary, however
{
  outputs = _: {
    nixosModules.default = { };
  };
}
