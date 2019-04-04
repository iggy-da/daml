# This file has been generated by node2nix 1.6.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {
    "abbrev-1.1.1" = {
      name = "abbrev";
      packageName = "abbrev";
      version = "1.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/abbrev/-/abbrev-1.1.1.tgz";
        sha512 = "nne9/IiQ/hzIhY6pdDnbBtz7DjPTKrY00P/zvPSm5pOFkl6xuGrGnXn/VtTNNfNtAfZ9/1RtehkszU9qcTii0Q==";
      };
    };
    "ansi-styles-3.2.1" = {
      name = "ansi-styles";
      packageName = "ansi-styles";
      version = "3.2.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-3.2.1.tgz";
        sha512 = "VT0ZI6kZRdTh8YyJw3SMbYm/u+NqfsAxEpWO0Pf9sq8/e94WxxOpPKx9FR1FlyCtOVDNOQ+8ntlqFxiRc+r5qA==";
      };
    };
    "array-find-index-1.0.2" = {
      name = "array-find-index";
      packageName = "array-find-index";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/array-find-index/-/array-find-index-1.0.2.tgz";
        sha1 = "df010aa1287e164bbda6f9723b0a96a1ec4187a1";
      };
    };
    "asap-2.0.6" = {
      name = "asap";
      packageName = "asap";
      version = "2.0.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/asap/-/asap-2.0.6.tgz";
        sha1 = "e50347611d7e690943208bbdafebcbc2fb866d46";
      };
    };
    "balanced-match-1.0.0" = {
      name = "balanced-match";
      packageName = "balanced-match";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.0.tgz";
        sha1 = "89b4d199ab2bee49de164ea02b89ce462d71b767";
      };
    };
    "brace-expansion-1.1.11" = {
      name = "brace-expansion";
      packageName = "brace-expansion";
      version = "1.1.11";
      src = fetchurl {
        url = "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz";
        sha512 = "iCuPHDFgrHX7H2vEI/5xpz07zSHB00TpugqhmYtVmMO6518mCuRMoOYFldEBl0g187ufozdaHgWKcYFb61qGiA==";
      };
    };
    "builtin-modules-1.1.1" = {
      name = "builtin-modules";
      packageName = "builtin-modules";
      version = "1.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/builtin-modules/-/builtin-modules-1.1.1.tgz";
        sha1 = "270f076c5a72c02f5b65a47df94c5fe3a278892f";
      };
    };
    "chalk-2.4.1" = {
      name = "chalk";
      packageName = "chalk";
      version = "2.4.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/chalk/-/chalk-2.4.1.tgz";
        sha512 = "ObN6h1v2fTJSmUXoS3nMQ92LbDK9be4TV+6G+omQlGJFdcUX5heKi1LZ1YnRMIgwTLEj3E24bT6tYni50rlCfQ==";
      };
    };
    "color-convert-1.9.3" = {
      name = "color-convert";
      packageName = "color-convert";
      version = "1.9.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/color-convert/-/color-convert-1.9.3.tgz";
        sha512 = "QfAUtd+vFdAtFQcC8CCyYt1fYWxSqAiK2cSD6zDB8N3cpsEBAvRxp9zOGg6G/SHHJYAT88/az/IuDGALsNVbGg==";
      };
    };
    "color-name-1.1.3" = {
      name = "color-name";
      packageName = "color-name";
      version = "1.1.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/color-name/-/color-name-1.1.3.tgz";
        sha1 = "a7d0558bd89c42f795dd42328f740831ca53bc25";
      };
    };
    "concat-map-0.0.1" = {
      name = "concat-map";
      packageName = "concat-map";
      version = "0.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz";
        sha1 = "d8a96bd77fd68df7793a73036a3ba0d5405d477b";
      };
    };
    "debug-3.2.6" = {
      name = "debug";
      packageName = "debug";
      version = "3.2.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/debug/-/debug-3.2.6.tgz";
        sha512 = "mel+jf7nrtEl5Pn1Qx46zARXKDpBbvzezse7p7LqINmdoIk8PYP5SySaxEmYv6TZ0JyEKA1hsCId6DIhgITtWQ==";
      };
    };
    "debuglog-1.0.1" = {
      name = "debuglog";
      packageName = "debuglog";
      version = "1.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/debuglog/-/debuglog-1.0.1.tgz";
        sha1 = "aa24ffb9ac3df9a2351837cfb2d279360cd78492";
      };
    };
    "dezalgo-1.0.3" = {
      name = "dezalgo";
      packageName = "dezalgo";
      version = "1.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/dezalgo/-/dezalgo-1.0.3.tgz";
        sha1 = "7f742de066fc748bc8db820569dddce49bf0d456";
      };
    };
    "escape-string-regexp-1.0.5" = {
      name = "escape-string-regexp";
      packageName = "escape-string-regexp";
      version = "1.0.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-1.0.5.tgz";
        sha1 = "1b61c0562190a8dff6ae3bb2cf0200ca130b86d4";
      };
    };
    "fs.realpath-1.0.0" = {
      name = "fs.realpath";
      packageName = "fs.realpath";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz";
        sha1 = "1504ad2523158caa40db4a2787cb01411994ea4f";
      };
    };
    "glob-7.1.3" = {
      name = "glob";
      packageName = "glob";
      version = "7.1.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/glob/-/glob-7.1.3.tgz";
        sha512 = "vcfuiIxogLV4DlGBHIUOwI0IbrJ8HWPc4MU7HzviGeNho/UJDfi6B5p3sHeWIQ0KGIU0Jpxi5ZHxemQfLkkAwQ==";
      };
    };
    "graceful-fs-4.1.15" = {
      name = "graceful-fs";
      packageName = "graceful-fs";
      version = "4.1.15";
      src = fetchurl {
        url = "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.1.15.tgz";
        sha512 = "6uHUhOPEBgQ24HM+r6b/QwWfZq+yiFcipKFrOFiBEnWdy5sdzYoi+pJeQaPI5qOLRFqWmAXUPQNsielzdLoecA==";
      };
    };
    "has-flag-3.0.0" = {
      name = "has-flag";
      packageName = "has-flag";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/has-flag/-/has-flag-3.0.0.tgz";
        sha1 = "b5d454dc2199ae225699f3467e5a07f3b955bafd";
      };
    };
    "hosted-git-info-2.7.1" = {
      name = "hosted-git-info";
      packageName = "hosted-git-info";
      version = "2.7.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/hosted-git-info/-/hosted-git-info-2.7.1.tgz";
        sha512 = "7T/BxH19zbcCTa8XkMlbK5lTo1WtgkFi3GvdWEyNuc4Vex7/9Dqbnpsf4JMydcfj9HCg4zUWFTL3Za6lapg5/w==";
      };
    };
    "inflight-1.0.6" = {
      name = "inflight";
      packageName = "inflight";
      version = "1.0.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz";
        sha1 = "49bd6331d7d02d0c09bc910a1075ba8165b56df9";
      };
    };
    "inherits-2.0.3" = {
      name = "inherits";
      packageName = "inherits";
      version = "2.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/inherits/-/inherits-2.0.3.tgz";
        sha1 = "633c2c83e3da42a502f52466022480f4208261de";
      };
    };
    "is-builtin-module-1.0.0" = {
      name = "is-builtin-module";
      packageName = "is-builtin-module";
      version = "1.0.0";
      src = fetchurl {
        url = "http://registry.npmjs.org/is-builtin-module/-/is-builtin-module-1.0.0.tgz";
        sha1 = "540572d34f7ac3119f8f76c30cbc1b1e037affbe";
      };
    };
    "json-parse-better-errors-1.0.2" = {
      name = "json-parse-better-errors";
      packageName = "json-parse-better-errors";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/json-parse-better-errors/-/json-parse-better-errors-1.0.2.tgz";
        sha512 = "mrqyZKfX5EhL7hvqcV6WG1yYjnjeuYDzDhhcAAUrq8Po85NBQBJP+ZDUT75qZQ98IkUoBqdkExkukOU7Ts2wrw==";
      };
    };
    "minimatch-3.0.4" = {
      name = "minimatch";
      packageName = "minimatch";
      version = "3.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/minimatch/-/minimatch-3.0.4.tgz";
        sha512 = "yJHVQEhyqPLUTgt9B83PXu6W3rx4MvvHvSUvToogpwoGDOUQ+yDrR0HRot+yOCdCO7u4hX3pWft6kWBBcqh0UA==";
      };
    };
    "minimist-0.0.8" = {
      name = "minimist";
      packageName = "minimist";
      version = "0.0.8";
      src = fetchurl {
        url = "http://registry.npmjs.org/minimist/-/minimist-0.0.8.tgz";
        sha1 = "857fcabfc3397d2625b8228262e86aa7a011b05d";
      };
    };
    "mkdirp-0.5.1" = {
      name = "mkdirp";
      packageName = "mkdirp";
      version = "0.5.1";
      src = fetchurl {
        url = "http://registry.npmjs.org/mkdirp/-/mkdirp-0.5.1.tgz";
        sha1 = "30057438eac6cf7f8c4767f38648d6697d75c903";
      };
    };
    "ms-2.1.1" = {
      name = "ms";
      packageName = "ms";
      version = "2.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/ms/-/ms-2.1.1.tgz";
        sha512 = "tgp+dl5cGk28utYktBsrFqA7HKgrhgPsg6Z/EfhWI4gl1Hwq8B/GmY/0oXZ6nF8hDVesS/FpnYaD/kOWhYQvyg==";
      };
    };
    "nopt-4.0.1" = {
      name = "nopt";
      packageName = "nopt";
      version = "4.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/nopt/-/nopt-4.0.1.tgz";
        sha1 = "d0d4685afd5415193c8c7505602d0d17cd64474d";
      };
    };
    "normalize-package-data-2.4.0" = {
      name = "normalize-package-data";
      packageName = "normalize-package-data";
      version = "2.4.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/normalize-package-data/-/normalize-package-data-2.4.0.tgz";
        sha512 = "9jjUFbTPfEy3R/ad/2oNbKtW9Hgovl5O1FvFWKkKblNXoN/Oou6+9+KKohPK13Yc3/TyunyWhJp6gvRNR/PPAw==";
      };
    };
    "once-1.4.0" = {
      name = "once";
      packageName = "once";
      version = "1.4.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/once/-/once-1.4.0.tgz";
        sha1 = "583b1aa775961d4b113ac17d9c50baef9dd76bd1";
      };
    };
    "os-homedir-1.0.2" = {
      name = "os-homedir";
      packageName = "os-homedir";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/os-homedir/-/os-homedir-1.0.2.tgz";
        sha1 = "ffbc4988336e0e833de0c168c7ef152121aa7fb3";
      };
    };
    "os-tmpdir-1.0.2" = {
      name = "os-tmpdir";
      packageName = "os-tmpdir";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/os-tmpdir/-/os-tmpdir-1.0.2.tgz";
        sha1 = "bbe67406c79aa85c5cfec766fe5734555dfa1274";
      };
    };
    "osenv-0.1.5" = {
      name = "osenv";
      packageName = "osenv";
      version = "0.1.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/osenv/-/osenv-0.1.5.tgz";
        sha512 = "0CWcCECdMVc2Rw3U5w9ZjqX6ga6ubk1xDVKxtBQPK7wis/0F2r9T6k4ydGYhecl7YUBxBVxhL5oisPsNxAPe2g==";
      };
    };
    "path-is-absolute-1.0.1" = {
      name = "path-is-absolute";
      packageName = "path-is-absolute";
      version = "1.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz";
        sha1 = "174b9268735534ffbc7ace6bf53a5a9e1b5c5f5f";
      };
    };
    "read-installed-4.0.3" = {
      name = "read-installed";
      packageName = "read-installed";
      version = "4.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/read-installed/-/read-installed-4.0.3.tgz";
        sha1 = "ff9b8b67f187d1e4c29b9feb31f6b223acd19067";
      };
    };
    "read-package-json-2.0.13" = {
      name = "read-package-json";
      packageName = "read-package-json";
      version = "2.0.13";
      src = fetchurl {
        url = "https://registry.npmjs.org/read-package-json/-/read-package-json-2.0.13.tgz";
        sha512 = "/1dZ7TRZvGrYqE0UAfN6qQb5GYBsNcqS1C0tNK601CFOJmtHI7NIGXwetEPU/OtoFHZL3hDxm4rolFFVE9Bnmg==";
      };
    };
    "readdir-scoped-modules-1.0.2" = {
      name = "readdir-scoped-modules";
      packageName = "readdir-scoped-modules";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/readdir-scoped-modules/-/readdir-scoped-modules-1.0.2.tgz";
        sha1 = "9fafa37d286be5d92cbaebdee030dc9b5f406747";
      };
    };
    "semver-5.6.0" = {
      name = "semver";
      packageName = "semver";
      version = "5.6.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/semver/-/semver-5.6.0.tgz";
        sha512 = "RS9R6R35NYgQn++fkDWaOmqGoj4Ek9gGs+DPxNUZKuwE183xjJroKvyo1IzVFeXvUrvmALy6FWD5xrdJT25gMg==";
      };
    };
    "slash-1.0.0" = {
      name = "slash";
      packageName = "slash";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/slash/-/slash-1.0.0.tgz";
        sha1 = "c41f2f6c39fc16d1cd17ad4b5d896114ae470d55";
      };
    };
    "slide-1.1.6" = {
      name = "slide";
      packageName = "slide";
      version = "1.1.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/slide/-/slide-1.1.6.tgz";
        sha1 = "56eb027d65b4d2dce6cb2e2d32c4d4afc9e1d707";
      };
    };
    "spdx-0.5.2" = {
      name = "spdx";
      packageName = "spdx";
      version = "0.5.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/spdx/-/spdx-0.5.2.tgz";
        sha512 = "WQbfCQT2uKLsDllnO9ItpcGUiiF1O/ZvBGCyqFZRg122HgiZubpwpZiM7BkmH19HC3XR3Z+DFMGJNzXSPebG8A==";
      };
    };
    "spdx-compare-1.0.0" = {
      name = "spdx-compare";
      packageName = "spdx-compare";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/spdx-compare/-/spdx-compare-1.0.0.tgz";
        sha512 = "C1mDZOX0hnu0ep9dfmuoi03+eOdDoz2yvK79RxbcrVEG1NO1Ph35yW102DHWKN4pk80nwCgeMmSY5L25VE4D9A==";
      };
    };
    "spdx-correct-3.0.2" = {
      name = "spdx-correct";
      packageName = "spdx-correct";
      version = "3.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/spdx-correct/-/spdx-correct-3.0.2.tgz";
        sha512 = "q9hedtzyXHr5S0A1vEPoK/7l8NpfkFYTq6iCY+Pno2ZbdZR6WexZFtqeVGkGxW3TEJMN914Z55EnAGMmenlIQQ==";
      };
    };
    "spdx-exceptions-1.0.5" = {
      name = "spdx-exceptions";
      packageName = "spdx-exceptions";
      version = "1.0.5";
      src = fetchurl {
        url = "http://registry.npmjs.org/spdx-exceptions/-/spdx-exceptions-1.0.5.tgz";
        sha1 = "9d21ac4da4bdb71d060fb74e5a67531d032cbba6";
      };
    };
    "spdx-exceptions-2.2.0" = {
      name = "spdx-exceptions";
      packageName = "spdx-exceptions";
      version = "2.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/spdx-exceptions/-/spdx-exceptions-2.2.0.tgz";
        sha512 = "2XQACfElKi9SlVb1CYadKDXvoajPgBVPn/gOQLrTvHdElaVhr7ZEbqJaRnJLVNeaI4cMEAgVCeBMKF6MWRDCRA==";
      };
    };
    "spdx-expression-parse-3.0.0" = {
      name = "spdx-expression-parse";
      packageName = "spdx-expression-parse";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/spdx-expression-parse/-/spdx-expression-parse-3.0.0.tgz";
        sha512 = "Yg6D3XpRD4kkOmTpdgbUiEJFKghJH03fiC1OPll5h/0sO6neh2jqRDVHOQ4o/LMea0tgCkbMgea5ip/e+MkWyg==";
      };
    };
    "spdx-license-ids-1.2.2" = {
      name = "spdx-license-ids";
      packageName = "spdx-license-ids";
      version = "1.2.2";
      src = fetchurl {
        url = "http://registry.npmjs.org/spdx-license-ids/-/spdx-license-ids-1.2.2.tgz";
        sha1 = "c9df7a3424594ade6bd11900d596696dc06bac57";
      };
    };
    "spdx-license-ids-3.0.2" = {
      name = "spdx-license-ids";
      packageName = "spdx-license-ids";
      version = "3.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/spdx-license-ids/-/spdx-license-ids-3.0.2.tgz";
        sha512 = "qky9CVt0lVIECkEsYbNILVnPvycuEBkXoMFLRWsREkomQLevYhtRKC+R91a5TOAQ3bCMjikRwhyaRqj1VYatYg==";
      };
    };
    "spdx-ranges-2.0.0" = {
      name = "spdx-ranges";
      packageName = "spdx-ranges";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/spdx-ranges/-/spdx-ranges-2.0.0.tgz";
        sha512 = "AUUXLfqkwD7GlzZkXv8ePPCpPjeVWI9xJCfysL8re/uKb6H10umMnC7bFRsHmLJan4fslUtekAgpHlSgLc/7mA==";
      };
    };
    "spdx-satisfies-4.0.0" = {
      name = "spdx-satisfies";
      packageName = "spdx-satisfies";
      version = "4.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/spdx-satisfies/-/spdx-satisfies-4.0.0.tgz";
        sha512 = "OcARj6U1OuVv98SVrRqgrR30sVocONtoPpnX8Xz4vXNrFVedqtbgkA+0KmQoXIQ2xjfltPPRVIMeNzKEFLWWKQ==";
      };
    };
    "supports-color-5.5.0" = {
      name = "supports-color";
      packageName = "supports-color";
      version = "5.5.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/supports-color/-/supports-color-5.5.0.tgz";
        sha512 = "QjVjwdXIt408MIiAqCX4oUKsgU2EqAGzs2Ppkm4aQYbjm+ZEWEcW4SfFNTr4uMNZma0ey4f5lgLrkB0aX0QMow==";
      };
    };
    "treeify-1.1.0" = {
      name = "treeify";
      packageName = "treeify";
      version = "1.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/treeify/-/treeify-1.1.0.tgz";
        sha512 = "1m4RA7xVAJrSGrrXGs0L3YTwyvBs2S8PbRHaLZAkFw7JR8oIFwYtysxlBZhYIa7xSyiYJKZ3iGrrk55cGA3i9A==";
      };
    };
    "util-extend-1.0.3" = {
      name = "util-extend";
      packageName = "util-extend";
      version = "1.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/util-extend/-/util-extend-1.0.3.tgz";
        sha1 = "a7c216d267545169637b3b6edc6ca9119e2ff93f";
      };
    };
    "validate-npm-package-license-3.0.4" = {
      name = "validate-npm-package-license";
      packageName = "validate-npm-package-license";
      version = "3.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/validate-npm-package-license/-/validate-npm-package-license-3.0.4.tgz";
        sha512 = "DpKm2Ui/xN7/HQKCtpZxoRWBhZ9Z0kqtygG8XCgNQ8ZlDnxuQmWhj566j8fN4Cu3/JmbhsDo7fcAJq4s9h27Ew==";
      };
    };
    "wrappy-1.0.2" = {
      name = "wrappy";
      packageName = "wrappy";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz";
        sha1 = "b5243d8f3ec1aa35f1364605bc0d1036e30ab69f";
      };
    };
  };
in
{
  license-checker = nodeEnv.buildNodePackage {
    name = "license-checker";
    packageName = "license-checker";
    version = "24.0.1";
    src = fetchurl {
      url = "https://registry.npmjs.org/license-checker/-/license-checker-24.0.1.tgz";
      sha512 = "9qgTmu3aAk4e2L6fWQLLVcip/tZJ0x2K8ZfmEIo8nsB3cibSw/Lp/YplBiOOI3SfnEfLb50T3rDq3X6gai4dEw==";
    };
    dependencies = [
      sources."abbrev-1.1.1"
      sources."ansi-styles-3.2.1"
      sources."array-find-index-1.0.2"
      sources."asap-2.0.6"
      sources."balanced-match-1.0.0"
      sources."brace-expansion-1.1.11"
      sources."builtin-modules-1.1.1"
      sources."chalk-2.4.1"
      sources."color-convert-1.9.3"
      sources."color-name-1.1.3"
      sources."concat-map-0.0.1"
      sources."debug-3.2.6"
      sources."debuglog-1.0.1"
      sources."dezalgo-1.0.3"
      sources."escape-string-regexp-1.0.5"
      sources."fs.realpath-1.0.0"
      sources."glob-7.1.3"
      sources."graceful-fs-4.1.15"
      sources."has-flag-3.0.0"
      sources."hosted-git-info-2.7.1"
      sources."inflight-1.0.6"
      sources."inherits-2.0.3"
      sources."is-builtin-module-1.0.0"
      sources."json-parse-better-errors-1.0.2"
      sources."minimatch-3.0.4"
      sources."minimist-0.0.8"
      sources."mkdirp-0.5.1"
      sources."ms-2.1.1"
      sources."nopt-4.0.1"
      sources."normalize-package-data-2.4.0"
      sources."once-1.4.0"
      sources."os-homedir-1.0.2"
      sources."os-tmpdir-1.0.2"
      sources."osenv-0.1.5"
      sources."path-is-absolute-1.0.1"
      sources."read-installed-4.0.3"
      sources."read-package-json-2.0.13"
      sources."readdir-scoped-modules-1.0.2"
      sources."semver-5.6.0"
      sources."slash-1.0.0"
      sources."slide-1.1.6"
      (sources."spdx-0.5.2" // {
        dependencies = [
          sources."spdx-exceptions-1.0.5"
          sources."spdx-license-ids-1.2.2"
        ];
      })
      sources."spdx-compare-1.0.0"
      sources."spdx-correct-3.0.2"
      sources."spdx-exceptions-2.2.0"
      sources."spdx-expression-parse-3.0.0"
      sources."spdx-license-ids-3.0.2"
      sources."spdx-ranges-2.0.0"
      sources."spdx-satisfies-4.0.0"
      sources."supports-color-5.5.0"
      sources."treeify-1.1.0"
      sources."util-extend-1.0.3"
      sources."validate-npm-package-license-3.0.4"
      sources."wrappy-1.0.2"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "Check license info for a pacakge";
      homepage = "https://github.com/davglass/license-checker#readme";
      license = "BSD-3-Clause";
    };
    production = true;
    bypassCache = true;
  };
}