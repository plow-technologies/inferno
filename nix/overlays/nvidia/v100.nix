final: prev: {
  # Override the default NVIDIA driver with one known to work well with
  # the V100 GPU
  linuxPackages = prev.linuxPackages // {
    nvidia_x11 = prev.linuxPackages.nvidia_x11.overrideAttrs (
      old:
      let
        version = "525.147.05";
      in
      {
        inherit version;
        src = final.fetchurl {
          url = "https://us.download.nvidia.com/tesla/${version}/NVIDIA-Linux-x86_64-${version}.run";
          sha256 = "sha256-Q1GD6lRcfhLjBE15htoHdYozab7+fuUZ6zsGPUrz/vE=";
        };
      }
    );
  };
}
