final: prev: {
  # Override the default NVIDIA driver with one known to work well with
  # CUDA
  linuxPackages = prev.linuxPackages // {
    nvidia_x11 = prev.linuxPackages.nvidia_x11.overrideAttrs (
      old:
      let
        version = "535.183.01";
      in
      {
        inherit version;
        src = final.fetchurl {
          url = "https://us.download.nvidia.com/tesla/${version}/NVIDIA-Linux-x86_64-${version}.run";
          sha256 = "sha256-9nB6+92pQH48vC5RKOYLy82/AvrimVjHL6+11AXouIM=";
        };
      }
    );
  };
}
