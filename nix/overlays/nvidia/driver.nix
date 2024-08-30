final: prev: {
  # Override the default NVIDIA driver with one known to work well with
  # CUDA
  linuxPackages = prev.linuxPackages // {
    nvidia_x11 = prev.linuxPackages.nvidia_x11.overrideAttrs (
      old:
      let
        version = "515.105.01";
      in
      {
        inherit version;
        src = final.fetchurl {
          url = "https://us.download.nvidia.com/tesla/${version}/NVIDIA-Linux-x86_64-${version}.run";
          sha256 = "sha256-0000000000000000000000000000000000000000000=";
        };
      }
    );
  };
}
