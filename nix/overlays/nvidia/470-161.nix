final: prev: {
  # Override the default NVIDIA driver with one known to work well with
  # CUDA
  linuxPackages = prev.linuxPackages // {
    nvidia_x11 = prev.linuxPackages.nvidia_x11.overrideAttrs (
      old:
      let
        version = "470.161.03";
      in
      {
        inherit version;
        src = final.fetchurl {
          url = "https://us.download.nvidia.com/tesla/${version}/NVIDIA-Linux-x86_64-${version}.run";
          sha256 = "sha256-Xagqf4x254Hn1/C+e3mNtNNE8mvU+s+avPPHHHH+dkA=";
        };
      }
    );
  };
}
