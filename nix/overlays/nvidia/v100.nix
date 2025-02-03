final: prev: {
  # Override the default NVIDIA driver with one known to work well with
  # the V100 GPU
  linuxPackages_5_4 = prev.linuxPackages_5_4 // {
    nvidia_x11 =
      let
        # nvidia = prev.linuxPackages_5_10.nvidia_x11.override { stdenv = prev.gcc11Stdenv; };
        nvidia = prev.linuxPackages_5_4.nvidia_x11;
      in
      nvidia.overrideAttrs (
        old:
        let
          version = "515.105.01";
        in
        {
          inherit version;
          src = final.fetchurl {
            url = "https://us.download.nvidia.com/tesla/${version}/NVIDIA-Linux-x86_64-${version}.run";
            sha256 = "sha256-ndIiHybIR8hk3+gMyFM/MixfTfqik5z1SpNLj3ovag0=";
          };
        }
      );
  };
}
