-module(flow_pools_cron).

% {
%   "S5iWl7L76dBpvrm-i_SRWODchdLYBGwYeSrIxC8v-vI": "Botega LP ARIO/wAR",
%   "8sCebZHN8C5eheRWF243TFCF84OLqGWBYP8PdKf8NK8": "Botega LP AO/ARIO",
%   "Lji25RS3-9LXZe4-gFpdnBMgqyRDqUzZPHWS-HqPWsU": "Botega LP FOREST/MAR",
%   "cG2uakimBR65Yk2YxPqBYfnONVosuPXD5_8j4lNa2Fs": "Botega LP AO/ELEPHAS",
%   "7zFWm1xqJ1u1slsvOcNbeyLCzxozOmyVJeRJjOH_i4I": "Botega LP HUEL/AO",
%   "avosk_bDmNBOq0H4XgQ7aCDvcndI1OCglWc1OIAq9hI": "Botega LP PORT/MAR",
%   "caLDjssasuUw389rjc-kJGsU-bEFgnGTBBTJoFRNBvM": "Botega LP DUMDUM/wUSDC",
%   "ttOZLNyBZokWYmAlNIDqngzYXj9sEIU22B0sBTJobWc": "Botega LP BLAH/wAR",
%   "wLtGns2uIfcwTDm6QXeiL5QQa3tQPiI-Aztr5aqdyiA": "Botega LP BLAH/wAR",
%   "20cEmow3vtkcmGbqgDIq7J9iLJB46_ku_LUoLvQT3uY": "Botega LP WYOG/AODOGE",
%   "0yR7PW8sCb_SuOgf0s8t1xX0-X2pqLnm6fEF776jvgo": "Botega LP wAR/ARIO",
%   "O0WPt9S6HQXYQLdRty9q62VhzlTw9E-eBl48O8BKoXs": "Botega LP wAR/MATRIX",
%   "Bv5mfnx5Ln2BU60inXPnMOGMwecJXadV4oqw7iwjzSk": "Botega LP wAR/qAR",
%   "BHxdWnUKGlSjSOe5M6gDja4zUokHLaqoH7ykWS2y_aA": "Botega LP qAR/MICHI",
%   "bOa5Szy9HbHaNPSrYirfHJzfIU2-Jzx9M_sHJL3BZew": "Botega LP FPT/TUSDA",
%   "t9ZrPXrahkITvI0dEEJm_tfH5eAnkHeks9l0k1-VwhU": "Botega LP DAM/qAR",
%   "WNaohFi8zrRKo4fYFicO2mmUgkp3guL_G-QLlLwi7CI": "Botega LP HH/wAR",
%   "XtqSDtI9dsxx4ZJIR6o5VAu-ZGb0CBkwM8sxbYL9DoA": "Botega LP wUSDC/AO",
%   "uQ6ZBNCJVb3SdgXWtsXT85JFXoqplo6Ocq5IE76ROk4": "Botega LP MV01/AOB",
%   "QRRmJbYqbCCO0o2y7u9vBhiqONpma-EAnXadf6Zkxyw": "Botega LP AGENT/AO",
%   "uay9sRihdYZOQxX9_tv7-ac4RDKHowZhBg2zscniQqo": "Botega LP NAB/BCAT",
%   "ciXarE4hh0suXPwpkGmNYh33AUhm88rfz_2guhK6SeM": "Botega LP NAB/wUSDC",
%   "2fG2_DZWLmEeAzWilPMgIXx9Zpho0kubDnBuLocxsRQ": "Botega LP NAB/wUSDC",
%   "HuyireMveI5efFuMS7Bq4ZLJoo1Cp5Mp20bLALnuJKo": "Botega LP AO/SEND",
%   "cM161FtOkq1elIe6QvdP93RoN7NYKUjyWbm4HBeJgMw": "Botega LP MONKS/MAR",
%   "Z2y50NZ-DjIzQnjTDv-tlqPj9hCQW881S5kNy3nCzWk": "Botega LP SEND/AO",
%   "DzQ0ntwWzWRk-jYVxSSxQd-BVZ2cQgR1SMaS9mnMK5s": "Botega LP TOILET/AODOGE",
%   "aDeXXVoY5bYWk2k5SFK9bSIxDFw1M9d2ac2vRZEnW7s": "Botega LP ARIO/AO",
%   "Q9mnzqVuiEsCPcR_NrmqHnK5Foz4DnCFcVsB9nHh9yk": "Botega LP ARIO/qAR",
%   "VGqIZAMC6etpB8M_YDKL2wyCdUTEIY3dkJcfkwjI8RM": "Botega LP wAR/ARIO",
%   "Fsf8SFd7APT3sRQqI5mfKFh0QjaIis26ZvVuvzMQEH8": "Botega LP AO/ARIO",
%   "zxNI1XYPCjKTfFZ-kIMW_9pEGlleFf_FRZhRJBtvibk": "Botega LP AO/ARIO",
%   "C2wN-uUmTsyyKMc5qopJxS3-mlbGcH8G3BJIlFgHL78": "Botega LP AO/BULL",
%   "CW6tynwMQffgBeNE_g-a5hWZ_jUK3DF9sRcQS6k26Wk": "Botega LP GAME/wAR",
%   "9JgTfmz0d32tRSp4Z5ZNQygAPoDjWfEvdiWRsx4ECWU": "Botega LP wAR/HUEL",
%   "k7iek6czuasNpcvsM_xbJBckEL9qD8dMWsxTTCYD1I8": "Botega LP qAR/AO16Z",
%   "IJHmoKuKiGFzm3KDX2p3hMwVHAxGDdC2w8ow_2K2l5w": "Botega LP SEND/wAR",
%   "dItcG4lIVGSsi_wufAFeiWSkKCmZR25IbUuwykpQUhc": "Botega LP qAR/HRSE",
%   "Z4PyVHiRi1-lkatNCcZLTQZ2j3NSqemMfME3UyzQHtE": "Botega LP AOFI/wAR",
%   "Qngdn8yLIcqqmqI4nLDoXEwCHbnNRcoLN9v4tXkaXvI": "Botega LP AGENT/HALO",
%   "b-SCikkMXgH080uLOECwx-F2-sx6ZN2dfkbOB1rYeOA": "Botega LP qAR/RATTYB",
%   "lZf2035dMcEIN5sB_qCdmZAY48whU5DLseQrS3Htz9I": "Botega LP HWT/TUSDA",
%   "wvrRUDLrXmzeoIOk4vqOdjIMWG6NddkGG_GxoY6HHKg": "Botega LP AGENT/DUMPET",
%   "a2LzZL8hAVvdaY2ldjcSeI8RxrHMvN4D3q-7zDjnUkM": "Botega LP DUMPET/wAR",
%   "oqg_h_L6k4s9W1u3tNqTyo4DDDHiwCiAPSsV5nkG1Pc": "Botega LP wAR/LYNX",
%   "aHlTSg4UiAdmJ9i1LVslUuKHQOPG3wBhzIsLF9cGd8M": "Botega LP HRSE/RATTYB",
%   "0RrzrXCKNqkiTirTiHaAjqyZQ4DFTtiqBlhAYmidzaE": "Botega LP HRSE/HOG",
%   "nb5DzoJvO220ipghdr8MNg5aYZyrtOOloCCLt4h-CwA": "Botega LP BLAH/wAR",
%   "X_yN4PUVdrWRFAZFC5AaH-QXuz3-ZVPAU2q__jRdufw": "Botega LP EXP/MINT",
%   "nHWcfNzeqjZ8Bot4R-xs_7Jez7Fl2AeDzKlN7qWmS24": "Botega LP qAR/tIO",
%   "-K1S8zAZonuciOlSBJf6KwRaFbsUEvjazNk8JCbf81Q": "Botega LP HUEL/qAR",
%   "WReIFPff-HgJlqxCccFbZVbrksv8N8oZ-cVSh6BwG3c": "Botega LP TTYES/BRO2",
%   "9eM72ObMJM6o3WHi6nTldwhHsCXSKgzz1hv-FpURZB4": "Botega LP wAR/NAB",
%   "0Zgggcvri_G4C1U4AsTla1OoxW7Dp6-Qezps407VBfk": "Botega LP JHG/TUSDA",
%   "vDa7tuzarALNNG9obS1X5rMwq8EkxLZ4TmMKNjKcicg": "Botega LP HALO/DUMDUM",
%   "nEbZcVqabi5p_pqpPHH_0rIPU28rtlm-kLaH1ZRO1j4": "Botega LP wUSDC/BCAT",
%   "3dQhWgEPAlUAjpRNfNFGNLtXTpYFPaIqi7MCA_ZUn0M": "Botega LP AOFI/qAR",
%   "MiFZ25YrRhECseEJDhQJbqMl_WCmzx9cKBsEQoR7ClA": "Botega LP qAR/ELE",
%   "bcEDInEmT3hGZAsnvmvqNVm4rHdXntz-50YX0Ctt0KI": "Botega LP wAR/ELE",
%   "ZauxZTSokiZ5BjiOxKlbV5uv0l02ixm_-hD16FeVOBg": "Botega LP PAR/qAR",
%   "_AuOLZbC7maD41nCdX1i8ZEzgiVCqG6bCWyL7RcVWME": "Botega LP qAR/DOGE",
%   "6Pi7o68Bl6NYvr0nAkJHgEerT7vEbIiDwvk1BMFDhzk": "Botega LP AGENT/MOODENG",
%   "bxpz3u2USXv8Ictxb0aso3l8V9UTimaiGp9henzDsl8": "Botega LP wUSDC/NAB",
%   "zW-R3nHs6pIR1qMV1C-SPHG6UMRIdoV0IskV2IrN-ps": "Botega LP wAR/FKEEP",
%   "zN1qKpTdfVS-Y6qK7UiguIsZ_XdIsRw3cYbw-73GnC4": "Botega LP wAR/tIO",
%   "G-rNKwxLrZw5fjr3nF1FGJUVJn43XTZUVqg29G8Aaus": "Botega LP WDT/TUSDA",
%   "LPqr7Jh3DPMxttbCRoefEi3CRLI7qwVxpNRjUhGt0bw": "Botega LP AOFI/qAR",
%   "7kqquL0y4TGGjL6AJR3-nC0ZUNOK3ZT8IDeOHkRAaaY": "Botega LP qAR/HOG",
%   "I9j3syzwqBuFkORLj9TT2GzSLkRWE_RgYkJt7vhkui0": "Botega LP LKF/TUSDA",
%   "IejTY02J0CTX192BndBz_tMCgMmfclJ5IhX7Yp_pUlY": "Botega LP SEND/wAR",
%   "4N-avzg2oLBIRSQkox2Fb-dGoY7ip8rnNTdN03QuwvQ": "Botega LP SMITH/AGENT",
%   "b24BEv-rPc31MT7Jf4w3k9XUFID9_UN5p3uGvEmDnHA": "Botega LP btc/MAR",
%   "5PPjq38ysU6BoYRN0XLwdZXlIa-bYfH8SOYnoUg7isU": "Botega LP wAR/ANON",
%   "u138nDaxxc3BceXh89OAt7pRWb51bMuEdDYk67svHZ0": "Botega LP qAR/TRUMP",
%   "waJOrK6dYeqsprwb6itImF_U_sFP06PXPfSAh8P0NeI": "Botega LP LLAMA/BCAT",
%   "OmVtRDPHBYhTPhhjaI6vX-lx6qjmk83MQfEv6Vh5GsM": "Botega LP AGENT/DUMDUM",
%   "aoIUE7ky-ciffP6t_6aBIte09-k2H-G6mxjTFMIt51U": "Botega LP BIRDS/MAR",
%   "fhdbyMMNvdC2zQMgEa9BS3YOaXbuSt8RDrTt59Iw2uI": "Botega LP AO/TKTHB",
%   "FQgh4kSEKq_LTi0gtN72MM7gPo1BoqaLyj7n0vEDSYI": "Botega LP qAR/DUM",
%   "_MJbfFO79PJvVuKcqv9fF0_TABK2RlMcTJYHpyDB_8I": "Botega LP qAR/ALAON",
%   "ClDQh61QBKDNkQbrG-e7FzQvJIqWQkal9XSlNZRiWyI": "Botega LP wAR/DOGAO",
%   "43aVKPZ9ALIG5Dc646FyI4AAWCXnK9mZDCK1JhQL-tI": "Botega LP DUMDUM/EXP",
%   "6etPq6jNtY_lrGgtlJwMXYbU3TaMj3vy41QW6mjRkd0": "Botega LP EXP/AGENT",
%   "3GYFWSSiqLBkONTddwqw6Edfgl-ZdTarJlSIwPwj6Bc": "Botega LP wUSDC/tARIO",
%   "8XhvJZhVljxvLX2px2tyX94lieZ0maPTEuZjfq1DPdc": "Botega LP EXP/BCAT",
%   "geCORx-2C1Prnuvfpn-iuegyYaG-rI7taBI7GrOQicw": "Botega LP AITRUMP/qAR",
%   "Ahanqp8SNnH3fmQL9zWHBWEIA9ALqsFVNfP0J_1fNjw": "Botega LP wAR/TRUMP",
%   "tLfBD1vyCpxGu1O2OPuA3VuJ-IMZuTUcGKgU_I6AM7E": "Botega LP TRUMP/TRUNK",
%   "mR6ri1KnowZnAEEL_Gqdx06KEqnOl3BP8yrvG-lSgr8": "Botega LP wAR/CODE",
%   "KEZz6BIKcfGqSI947O-ZLAR6AJmpNIDS76W4bA2PZM8": "Botega LP RANDOM/MAR",
%   "lTZCuKY2up9N6m0cAX0efxYo_1MRyKNmqsqYBaWpS9U": "Botega LP 1kAR/MAR",
%   "aJ37bbyH8hOrh7urFNwq5vYsAHvjDtkTqf3jUF-2NwQ": "Botega LP wAR/NOKIT",
%   "di3SbaYlwnkvt4-X7MmPTTUm24bxJFn-p4GSAy0N-ko": "Botega LP AO/TKTHB",
%   "SKH_iMMYPaK9vycTf8yrFXtwk9DHcyKly9mj_MHcMJ8": "Botega LP AO/BRRBRR",
%   "otTTLgQ8Qn0X1pKJVNXwUrGfO6eBTbHNs4sqZ6pGjAg": "Botega LP B3TOKEN/wAR",
%   "EpaHewxQPcOyFGUZkuuN6VHIQUAhcDi3qrc1piTvpEE": "Botega LP AO/TKT",
%   "1P7Zl2FDdA09HTXDj0DxKatryPhRj4ES2JO8xXCcV78": "Botega LP wAR/X35BG",
%   "n6v-AxqxvxigDr2iH-tW1_q7z2kO6WMCudDESOCXKRg": "Botega LP wAR/JOOSE",
%   "fKWc1bhmZrPv8bUDtYqI-_zf2WJXL3gLFEzxOO6lan0": "Botega LP TST/TUSDA",
%   "oeoV6MKQHnB0WQjRUuQgtUeRkDzxU4DlSJm4u2LgNN4": "Botega LP qAR/RYB",
%   "aOeJAK6KKbcXbJ1ihbpGx3okJ7e_oZWNS-XcX5fYUCY": "Botega LP CATWEAVE/wAR",
%   "V-aAEX7HVu8AMOSqIhAR6myLrCZh2DOhX12OSYv5ypc": "Botega LP qAR/SEND",
%   "IO_1AR_hoU-pDG3SDgz7DKHbpcf_rl-m6hR0syPXu4Q": "Botega LP wAR/ARCHE",
%   "6dqYN-lE6E2w3c4TOjaTjiXMiXK6FJwGRp6jP4DqprU": "Botega LP MEMEA/DNE",
%   "G8Uff12pcgiLQxTvpLqRh-eYDoq90YZ0W0CqJj0RuKU": "Botega LP AO/TKTXX",
%   "6Js5EVtbmbWM_KvGi84cc-J1iNhmELUGWhIZshbey74": "Botega LP AO/BOOST",
%   "MdhKb-YoZC0I9Vn74ENhy3MKE7fHEHXRmYJk2ezVfEM": "Botega LP wUSDC/GAME",
%   "sDLmAWhX3FZej73SQtXzOr0FdIsF7RYis7smXkq_9qU": "Botega LP AO/TKT",
%   "dfihCE6Ml8UsCz6UkghyE51Y6zf5-WVRWq1eHJDUmV0": "Botega LP GAME/wAR",
%   "wDCJJwSRaLEUR8-80UwlzP2J0ex3TtGsss6OjMwThbE": "Botega LP BLAH/wAR",
%   "bMyl_dysjbyKJBbaT9u9RwgTd527H3WcSvVyGugljvM": "Botega LP DRPT/DDP",
%   "oLxEOaQMgTFvCdMzYKXZx3NKdVFoHJHgwdyrWuvxF3g": "Botega LP qAR/CATWEAVE",
%   "9AsRe9T7Q29HT9NRJo_aZwGzbjQ9zfJt8hL7YtHyxrk": "Botega LP qAR/NOKIT",
%   "zwf9Rs07dflF3GZgA7zK5W9uD8xsgEh2fMscvJAhf7Q": "Botega LP tARIO/HUEL",
%   "ZMde6bsol6kJJdWL_z90dK2stcyiyRkwpEhUblzY8u0": "Botega LP wAR/HAPPY",
%   "XPLl4BTGnwPDp9DKHfbBu-F9ea34oaAtx-Ip9UucFXM": "Botega LP NUTS/SXAO",
%   "WuAkqYvunUtsD8uvpsK9XtR7_5-fbnH3ATD4DJrl-Ac": "Botega LP DUMDUM/qAR",
%   "SbsFLYkHi4TDGpBIFhVFWzKwKn_CnP9U_b9jZS7hY_Y": "Botega LP DUMDUM/wAR",
%   "zEcRf8tq6CpeHDw_fa6cFLShujEysWT2Zy6XUu9CYZ8": "Botega LP TWIRL/AODOGE",
%   "BBiwIhEU9vxmSvtY0KkX5WTMudUcAS3-WKxAqIwoTKA": "Botega LP wAR/HOOD",
%   "oudDA-JUZi14kbKFIhu4X34qGYHbtZnHdtYWEqbtkO0": "Botega LP wUSDC/MINT",
%   "jo-1m8OUQT2GwjGPlZnWT6briVJitsTZF95M-FbjmZE": "Botega LP qAR/AO",
%   "bbbVW1d-ROSr-vr4fSwSYloUno2hTLBvuEFMOJq0zNA": "Botega LP btc3.0/MAR",
%   "1bMh-5Nr-vtB3PH4I9n_lO95Q6RarCLlcDH97V3TO8w": "Botega LP HALO/qAR",
%   "BttyF6xLfj55RcxxwRW_0Mf3hvVuTR2_PybpthAMPX0": "Botega LP TST/MAR",
%   "Oe-rQ8zQiI0SCE0Vy87luz_mDfxRD9-Go6yLlg_d3zA": "Botega LP wAR/BCAT",
%   "Z4DDOEw5BtZUOjv5NoPE-Aq0fQ0NZsfgJVDf74Lj0QE": "Botega LP GOOB/MAR",
%   "mx-2oRwKmgyHuEqVs8yQHnPoz_M-FAYkZm4meY3qnbg": "Botega LP JSL/MAR",
%   "FW4gOxJZjeY2Nu0BKtNnmRszufyogyislInWY--Z8UQ": "Botega LP MOC/MAR",
%   "y3qUfBn-pFyVrYE4LyHGo_wVcABrkkKz2FQbnIV1S2s": "Botega LP VMKS/MAR",
%   "cT-lzIxPC41nzfkqqmgRuj9csiUmz3Fa1Q2zOmsBwc8": "Botega LP wAR/NOKIT",
%   "cCbi4ww8P2vbliNUz1uS91TxJqwsCwE05Asso0-VLRA": "Botega LP wAR/MEAOW",
%   "FdslH-kOyMosNdJ8oYvuwOFjw__iz89bM0IxByZS3Oo": "Botega LP wAR/RAO",
%   "uSuvCl2GLgVuX5WTmBJfjcTe--yMyuXeuC5F-5Hboio": "Botega LP tARIO/TRUNK",
%   "JowM8fdYjAMw14QwyX8CmN2iO09HpzaQpb-d9tNHPT0": "Botega LP wAR/DUMDUM",
%   "b1S4T9d2ZGM5P0FIKg2HLC4rcbrYvg5xZsRZKC4SgvE": "Botega LP EXP/qAR",
%   "26BXDOZNPRhRwc7QFymTF5IJX-mBO2E8T8PN1Yj4olg": "Botega LP wUSDC/AO",
%   "O8mgmJoXJiibdXRdu8Ndze6BE5rYQ5bWW3KQuKzlRfU": "Botega LP BCAT/wAR",
%   "NyDgWkkkgOt0Y8cCuf15L0vlcmCkmmN9b6ZnIh1GZwY": "Botega LP CHEEK/MAR",
%   "Fntcs_1CZYNck6932-D35sHXxERe7soHFEZUwqq6anQ": "Botega LP qAR/DUMDUM",
%   "C8nhA7daxSLsvjljKRDGWLac54njWlXzvs0m2OKeE_4": "Botega LP AO/BCAT",
%   "SKRRMo9cgL-V1gzPJa86c6Xh54ipWIxftmH60p_GvEA": "Botega LP AO/TKT",
%   "mqtJ7Y133FDDyBA45pUFV_mJ5cfb49qn_Z2cEpfj29k": "Botega LP GAME/wAR",
%   "7lyQ5ShtM6DJ_KejWBHxbZtlVUNkpdZ1957wQHavuJM": "Botega LP SMONEY/AO",
%   "JkN_xrwtnNAaz3bk3XHVG2Dws5tXXT2cIZ49X0lRi1c": "Botega LP NAB/DUMDUM",
%   "KdIH8lNEuYEySqTYAoA_ceBuvgEKcmk6u_y8GMhBN80": "Botega LP AGENT/NAB",
%   "XUihw_LEASdnf1jaLr2hLvh3wJmNRaWjvgFn6kGlx8o": "Botega LP FFT/UN",
%   "FkG53NqW_qGkpEHlQgAswHgh3oXV-GzB4_p-FJ70DOM": "Botega LP wAR/wANS",
%   "ZBim0kTcHenQqqa-LYsqYP7v5yelbEvgxpG4FAaUiaY": "Botega LP TKT/wAR",
%   "wVv_arTkiiWVE-0vRImkPr34u-C-8Di6Wtxry0PKZiM": "Botega LP SMONEY/AO",
%   "eL1DtzYiy-Feq0nqa3WY0BUgJUy7NOgKBpJ3KfKn5fA": "Botega LP TKT6/TKT7",
%   "lwjEiao452i-SF_Z2rFxDJ-Hf9NXaBtO2P0pJnTK5TQ": "Botega LP wAR/LAZY",
%   "RFtwVGuyaOp5Z0tuLCSgH0ZQcFS6cVFR5NnlZTTWnY8": "Botega LP AGENT/ELE",
%   "t3Dxq0oH5gPH38T3uQAek1269tTwMSSZgU6lBFvrNEs": "Botega LP LLAMA/qAR",
%   "gJiY9XYhDkI-lrGwmRD85XDJLh2bgQDpSQ-G64GMrZ8": "Botega LP wAR/tEXP",
%   "4sua9ZsGUX1wsV1FdgKue0J84zJ2DQ1xbHdumg1iyiQ": "Botega LP wAR/TRUMPFAM",
%   "sXvC-SIRNZ5VuuYffbkRhuRzzoD9E0A_mIxMV_KqWS8": "Botega LP BCAT/RYB",
%   "qcdYZTJZHsn5JnwjOXLzO3P7HfXMHvpH_qsvcipGNkQ": "Botega LP wAR/RYB",
%   "LMMyJcy0gg9w3CvBexMNd1zfAtHYFYZoixsm1hRYMQE": "Botega LP SEND/NAB",
%   "OFSW21mXnnrzXyuRvBJP8DJ-wyj8hZMgcl5YgMYt1jM": "Botega LP qAR/RDR",
%   "wmRXi2tLDJ5da_nszdPj6b8SeS7P0Z4pbq9nDg18rPI": "Botega LP UPS/qAR",
%   "z-A8anZReyf0yRS3F8YZ3TsbNYt2RUE825VqRIbpHzw": "Botega LP AO/AGENT",
%   "yykwi3aZEs2FfDZRrNxWqThKYVcw2ddgohqQDDVrqxI": "Botega LP wAR/NAB",
%   "54OswXiutViNMa7AYopo9uxvMeE5dQ5cCYzy4Z_1Ruo": "Botega LP wAR/HB3",
%   "-kCXnJs0m1tn48JhjcwQwhGuKppqi9_62jLOWHYxuXA": "Botega LP wAR/BOOST",
%   "u--39jbZzEe4hx-e3Q3iy1w4reTDdblJlDzf5ilMRW4": "Botega LP wAR/BOOST",
%   "-XJfuETKr74ehh1GyZehkCzar9cSULETm8apCvLoAOU": "Botega LP wAR/BOOST",
%   "WMDAS97E57qXWSyZ8kpRYKH_iibiw0RcbX-IrVkE9dM": "Botega LP B3TOKEN/wAR",
%   "2QARxomUakctYq9GEXHCoYyw3YbL_8CHiHpzl-GJrTw": "Botega LP wAR/BOOST",
%   "XIFN3wtfUAGggtTC4zMU3Fe14gKH5qmMoBAUYht0jX4": "Botega LP wAR/BOOST",
%   "JNUjiiqjW__2S_lMC-6-yNAopTmtCyb_dQ2XcKXgYbI": "Botega LP AO/TKT",
%   "U5H5DzyKOAQ08Pgr5phBXNKRmjXqAiKj87je115xBTc": "Botega LP AO/TKT",
%   "H1jr9GvWghCSdTTNX-Xs2Dfi3R6Q9d5UEwumkNK_Go0": "Botega LP AO/TKTXX",
%   "sXrE9euzt4Tecr-4o4D2EJAZnqg5e3wc6Zs2DdMAO7c": "Botega LP TKT/wAR",
%   "pfZpmMbXyh6EUC5wEdAREmZ16uzBvjkE-tzTQbFizI8": "Botega LP TKT/AO",
%   "cZW1TCR0qtfrOSPStk5wTaoiB6gTzbcJQF8PSuBIwm4": "Botega LP AO/TKTXX",
%   "ZcQ4A-9mmK0VEHvDH9Y_45SPlyFAcnZ0noKgsHdQSLo": "Botega LP CATWEAVE/RYB",
%   "1gv__tYVymq5HpwvgSMUAHXtnxm6tiqgMisYp2-d4YA": "Botega LP MRTT/NAB",
%   "tFGbThZeg77tnpVb3Jt8pCOZBBce5J4ZZuZXRo3aOEg": "Botega LP wAR/owAR",
%   "DtKpboLR1mslsgdgA_sA2ALpPAvZR6AJERFcrRRf4UI": "Botega LP wAR/owAR",
%   "oVK6bSu-XA0Sgqh2ubAzJ5ZzBjleJjmR7L6ZgY53klM": "Botega LP wETH/owETH",
%   "RQTIq_IMiiA4GIOXcyzu0TZURK5xipsfOfsdEPEoHmE": "Botega LP wAR/owAR",
%   "yKngqSGQfxpfcXe-2RpCLoDY8ZTeC6jZuS3cQCYHo1Y": "Botega LP AO/wETH",
%   "rtDyHTj7aQvhLapDjiCLZIgUQVKgPlKm1_Civ7YYjVI": "Botega LP BLAH/wAR",
%   "xEJT833UXe3RnCM8NL1ov_bLSYbK1fxzsIauiiHFkNI": "Botega LP BLAH/wAR",
%   "rASh3F_II9K9srEE2dme11jGeTxyHiV7B7mx7CSaHX8": "Botega LP qAR/TKK",
%   "0h-5sRnIYjk4Xiyv6OoCxgVs2GNo2H4kN_mSAhfrpxM": "Botega LP wUSDC/EXP",
%   "qABj7_8uO60C33fAM3zIFqtl4Sq-t4cltOS-rUbX4pw": "Botega LP qAR/POOL",
%   "xoAj9HLWV6_kRnCAiGW8FzyiIdNKGXBnWa1Mq5IiByM": "Botega LP TRUNK/AO",
%   "T1hLoGQd82N1Npij8jwad4buDBCN-qF6_jE85ajMtBY": "Botega LP GBP/wUSDC",
%   "Ez4wPgeqcFyqmmF9UAaI-y9gzDuUwjZ0Vt1Y_DixFDQ": "Botega LP wAR/owAR",
%   "1Xw4ndvB9CPxeox658T75Slb2O4vpOhOuQVd_swnQkE": "Botega LP wAR/owAR",
%   "QyYyVEY5bdWU9s_Ms_Kk_IxIOnlLM6c-dAhL-BVq78Y": "Botega LP GBP/wAR",
%   "UmnJgkxvPmHna8FUl3-EW5_7vPH2TPJCtGItTYPjhF0": "Botega LP wAR/GBP",
%   "a3Z0v954hOPKZLuSZjQcgfyAG_zTNnhTQ81E_d_7zYA": "Botega LP 7i7o/qAR",
%   "ZJ_Zz1jD-5Fn-5PGom4UHZWFLOgm_J-kxRBtNI_7dQQ": "Botega LP wUSDC/owUSDC",
%   "gpNNHiPVV_SNTxQodRl8DE3QBZTg1596KeuQD0NNw_g": "Botega LP wUSDT/owUSDT",
%   "e8LHN_7sOYm2lhci-d1rJZc8L4EBgffE0zAwpIjRpb8": "Botega LP wAR/owAR",
%   "Mwe3OVJs-j_9YIpvDFCowNqnF_LnuEKxULT3VqApt1o": "Botega LP wAR/owAR",
%   "rG-b4gQwhfjnbmYhrnvCMDPuXguqmAmYwHZf4y24WYs": "Botega LP GAME/AO",
%   "5vrYhHQVL60Ll-DTB2gc4lwZoYUypk_FE-lDpXVprF4": "Botega LP NAB/MINT",
%   "wJerY4pm09KrMm18kO6fSzHJE4cOm6-hUZb_QBadxSA": "Botega LP MINT/GAME",
%   "qqFYnuevUj7FrWYU2OEM8zmxWUcG_yCdK5NyddDpMAg": "Botega LP ALAON/wAR",
%   "kv2cENbQba91pSfNaQ0YEZTTpG1F7ZnR30af_NWLt4k": "Botega LP AO/TKTN3",
%   "cvC0iZZwaXKHGdiNk_eafJfPACn9gLFE-Qk5qljVBuY": "Botega LP AO/RIP",
%   "_vuZD6v5Y1Ymty0gnSnAuz1_yi8Cbj4Rt6cNIJ9uosQ": "Botega LP AO/UPS",
%   "grzLTb5-CEdPUx59gPxtuohpvUWyse6s7j7OZqORLeY": "Botega LP MINT/GAME",
%   "X-IjsHfiTYtJSajp6lOByPsehIo_kjNluR7evM30Nrs": "Botega LP BLAH/wAR",
%   "56W8KtDS-EHcdmJr2LZfud7AI-ecqX87mmYoNxg3uz8": "Botega LP MOC/MAR",
%   "nvBNs4-46zlAehjkFqSQ2Tk7E4ys7dXpAEr8kqsySbc": "Botega LP BUNKER/AO",
%   "d8RyAhYPFi7alDcQGkFRFVFmmMeCsyVE0iTxPJk2Pd0": "Botega LP TKT/AO",
%   "l34zNwvGHXqvdAaymAQs9eNQPWccXQi3kQR0G-c4ei4": "Botega LP AO/RENOUNCE",
%   "oSJyr00y-501Rc_-As45d5CQVZA9YSn1GRYRy0QvAEo": "Botega LP AO/4FLOKKI4",
%   "yegjj-WALfaZLP2sqSdpmWJu6aLk1C2pYiPBUv2Ns2s": "Botega LP AO/SAGE",
%   "suMwoM-RO4n5QYXECfzjuO74IzsZNCqTNXXrFrrwG8c": "Botega LP AO/HB3",
%   "mDI_8W4nm329W7amUYk1jtpS4SNitL4BlWvJf8J5Ok0": "Botega LP wAR/SOCIALS",
%   "79ratRoWvTq937l5Z1EbLDhl2PfnkY9dOjoLFefncmI": "Botega LP AO/TKT",
%   "Eh2Sxp8-8LgBgrsC7QfKn9mlyjt28veIZ1iPxs8iANg": "Botega LP CVB/TUSDA",
%   "BNcegnV3HwDQ1oqXdBZ1xAIlOrykXaPPZ_Li_623yaM": "Botega LP TTT/TUSDA",
%   "g9SKT2nSmfw70CiQXcz03RTdg9SzwCc4qxAGFJ15xYk": "Botega LP AO/CHINA",
%   "uYHmatv2BSw85pB5j75mwbwKn_aMnpUqptoi6xIrjw0": "Botega LP AO/DRP",
%   "cV5woyP2L2tf4Cvi7HMGhJ4agVB4ZxFUkfzWSYVRlrU": "Botega LP AO/LLAMA",
%   "3rfChWnS2BAdyFaZAdfyiAmylD5fiynVA2-ugnGNBkg": "Botega LP DUMPET/qAR",
%   "G3GIcOVaKYh1Gr95SHRSajvG7-PZhWrm5F1gRldH-PM": "Botega LP wAR/AOZE",
%   "XHiV67PHKkBPYju1vl8X3soWVmrRb5TCZNxUF62b3nE": "Botega LP GOAT/wAR",
%   "Lt0PKHQCFxXkXjJVd5CV2tRIlXe55hs4cQ8_OY9JgsI": "Botega LP MINT/NAB",
%   "qV6mKrqEgMg-Lpr8VZAkcwWMUDJb6G0ANWcBf_2HtUU": "Botega LP qAR/FTX",
%   "-2O4bEiwnJu7MOpksKGlNvFAjH6XagtHfOo8d4ZRjaI": "Botega LP MCOL/MUSDC",
%   "vRuuTs9rqN3Wk6U7wyBlBJYq_MlBH94L1rCYPX2Oe0w": "Botega LP FFT/PNTS",
%   "N5yUAUA3XGKZmvoRXnYtJVvMu2wHun0Afrb6WuQCqk8": "Botega LP ARIO/NAB",
%   "XHdCpnwvsAe46bPR2B7Sz77zueZaHwoqptMX14RlSj0": "Botega LP ARIO/NAB",
%   "Ov64swLY1JfXK5nMFO-mc_Kb_s8vA6w2KDcjpho9BRU": "Botega LP TRD/DDP",
%   "wAxZpk1ZHezVlvm7xvYm4dDzI5DwSaaAaLPhHFxdr8w": "Botega LP qAR/MOODENG",
%   "EJX9HmxurbeXUCaoTTo38P0Wbc5mNrLNrfW3cSg_3rM": "Botega LP wAR/LLAMA",
%   "OuyYisy9BguYvrmFG-_kOsh7Zq4fw4RUGTZ0Z3X8FhA": "Botega LP wAR/TRUNK",
%   "-mtQOtKcmDs5aWc6Kp8EirHVHLimlcNpPchGbOGmE3Q": "Botega LP UN/PNTS",
%   "sCuP9nTQ8i1zWwl62z6bVnSpdzTyNnp3xVKqyGKX1rY": "Botega LP wAR/DATA",
%   "xrtZSjeU7-_pMJB6ytNmHcGq27xavxAOzfmym0mtdXI": "Botega LP wAR/NK",
%   "iIH6xBUYkh_IQ15iG24oBH7SZyIEtHV5ElpRiyUzqpk": "Botega LP NAB/SEND",
%   "4-90Nz2VYKwPvk5Fj-QbzWgLw5aAUASPRpetK8kjVpg": "Botega LP wAR/DUMDUM",
%   "LwVHULR9Xxr9-y0bjLPGYDvvsKQAlRPlXuFNWYhoSSI": "Botega LP BLAH/wAR",
%   "ZwzB73pKu1lQWdfBsBwjozxTyjacLF4fh_gLsAGcUfk": "Botega LP B3TOKEN/wAR",
%   "Aiqyy9wgpVnnd96rdCRzL-5La568Rr9cvRrd1Rcdar8": "Botega LP DUMDUM/qAR",
%   "3biQvRjIp_9Qz1L9D3SJ9laK4akCkP-8bvAo3pQ6jVI": "Botega LP AO/qAR",
%   "Zz_lYNe4_egM69eDvwBEURDmIiyYJd93dD3dxkN3V8Y": "Botega LP PNTS/PNTS",
%   "8lULl_gzuz212bWsMKMu6TavDLMDmAAtzPp90jmh4P8": "Botega LP PERMA/XYXZZ",
%   "EFGxspbF5cZEtsuXRZT1BSisOJfpYN9G0bIsDC5lyDI": "Botega LP wAR/PIXL",
%   "BGhaOrAQrel9KnzOMp82iWSJ49bKzTjUt65DyJvePk8": "Botega LP PNTS/ST2",
%   "M14d90SwY1pLZk8zq4OJxb5gFW-iaSMVNxZX2Lr02eI": "Botega LP ST2/UN",
%   "zYzUzy0ooaHj4eeFkBa3WdQE2CR7-nJ3WQteUnm6wMA": "Botega LP NAB/AO",
%   "aZtqKNPH7aZzB2Wu5yxe-oZz_sVKJ7HJwmayfKxIDOw": "Botega LP USDA-TST/TUSDA",
%   "QBXaaIbJRstLuAa8RSpsLEpuoMEaPn2dDlinygIyLB4": "Botega LP wAR/OPERATOR",
%   "oeDBkdZJpbSvc8AO7z5KfqBqFj1ULOMkcXyHY-MlYAc": "Botega LP wAR/DUM",
%   "xxwJCl8eh5FaUMZRCdAiaVP-KB_92RsouVIBsltpI6s": "Botega LP MAR/MAO",
%   "X4Bxg0tOex4MokUGmXN0na2RemJ_p7AShZyqTJV1glA": "Botega LP TKT/MAO",
%   "Uy7d7N6s08xE8zpBpkwNQ6Iay9I5-mZvVnTYwlB-548": "Botega LP qAR/JOOSE",
%   "TYqlQ2vqkF0H6nC0mCgGe6G12pqq9DsSXpvtHYc6_xY": "Botega LP wUSDC/AO",
%   "-Y43fXWysQS7r_eiz9T_xT4npzdFRmycs9VWtar2CXY": "Botega LP DUMDUM/AO",
%   "1Oz__qGSvdRx1kINc3G_BwEB2GzlG0rC0Yxe23Z-N-M": "Botega LP DUMDUM/AO",
%   "cfJIBVw4ojWnjiQJ1Z6CRU56YnYJPZpaMgAIs-BSi68": "Botega LP wAR/AO",
%   "R7PgfGuW241wiUPa6ZVngAz8eQ4QWJI-GQD-4jwJmPk": "Botega LP wAR/wUSDC",
%   "DhiYsqOtkd42rSlVyNOyJ8Clxv6HmFbIfi0fSz_PRik": "Botega LP qAR/wUSDC",
%   "GDwK5yJSSglzrjIe3aIXhSLK4UUzgITeDv-e_iBqh9Y": "Botega LP TKT/MAR",
%   "HmOAy8P5qKy5PlD3zlxYCW6RWE1spYPpCi0X9ZLARcI": "Botega LP TKT/TKT",
%   "2EshW1ysBFoiuN3n3HjkmTghZ5_MRktPmsDXZlPuoRI": "Botega LP DDT/DTR",
%   "5uwK4BEfcTy9h7jJQfAx4jlrJB827ANJbNIxHWGSNTU": "Botega LP TKT/TKT6",
%   "gnoqFV39NfY2WOt2dHMw0xfyuVl4KlgTiJcuqN0y7d4": "Botega LP AGENT/wUSDC",
%   "7sUFikrorGKlYyiy3XRoiRB6dc7f_3v_fkT2fbjocHE": "Botega LP wAR/VCAT",
%   "usMlh8eWf9wrLR9kSPosW--SrXJGZ40LC5Rekjgywy4": "Botega LP PIXL/qAR",
%   "cvL60xZdbvSpB7PVwX2K_P46loDvzDa7bjzjxlc3elo": "Botega LP qAR/GOAT",
%   "f90WPMT7E3ncJQYpyAUetV57weHG0ZPaK9Kp1uzIsbg": "Botega LP FFT/qAR",
%   "wTIpisZKMtG5WsLFqQBdoJEObyyeoJQmwWFm1h462D4": "Botega LP AO/qAR",
%   "NdeAIh-lymKjmg3TxDlvO2sroE-osq-Qumjgam8-XEE": "Botega LP AO/wAR",
%   "NgtjHNjEutLRm9MHRzRpGJj6K2aLp8wiUHeNZP-tSG8": "Botega LP qAR/wAR",
%   "hPmmc-3A4Ba0K6AiUUBH5Bo0hfAN4g8EJa9asRx06R0": "Botega LP AO/AGENT",
%   "B6qAwHi2OjZmyFCEU8hV6FZDSHbAOz8r0yy-fBbuTus": "Botega LP wAR/AO",
%   "8DdYrPbX0XM_3HuOqZN5zMphGFMSDG1pTMkWPyYJ_5k": "Botega LP AO/qAR",
%   "t8ewMNbdNlAhiOja3Ckep76SzLS7aPGK3L5CCCz7360": "Botega LP DAFD/wAR",
%   "huO1rEzyF3nPV7VyfFz7cHfvXamtbb7A98V34mJ_IJw": "Botega LP wAR/EXH",
%   "jrHt5iZL8qbrcwpdryLs37sYrIdg-CseI3hYrWs1f-s": "Botega LP DUMPET/RYB",
%   "k6_Gz94ZJoIowC-REQeiP5NXVafaH77pNJxui4_d9o0": "Botega LP RYB/CATWEAVE",
%   "vXGyFFsm5nug7L2pftopsvBW17f_BZoiNsTlecHjTKs": "Botega LP AZ/wAR",
%   "m8SixEouACAvoOiZJ4DrqkM1jhay2c8XX0WR7FhFUy0": "Botega LP RYB/GLX",
%   "uqk91fL2tOmMI_XYzE2cuk4bmE62gyWu_MNr-25L5Lk": "Botega LP wAR/CHARLIE",
%   "3Hb61h-FX6w0ntSRtSsP771fqLbSD_aZO9rZzp9g22o": "Botega LP ALPHA/qAR",
%   "PL-Ww9g3Pe0tW-Ozb9xclcwDCebDNT9MXFqLpkSTKlc": "Botega LP KJG/TUSDA",
%   "T9EBzA9-sQz_Xymfkv6XYM18NOLaMeEYHS-51FKzL7E": "Botega LP DTS/TUSDA",
%   "ukE6dU8O_RR9zNIfWaxbhP16VDYKibD2eGpJbDjoAA8": "Botega LP wAR/TAOT",
%   "lmaw9BhyycEIyxWhr0kF_tTcfoSoduDX8fChpHn2eQM": "Botega LP qAR/AGENT",
%   "N_JfhIr5Bwz6VTnbL0quOIzn4tgw3P-zxMo0jt6Mk1g": "Botega LP wAR/AGENT",
%   "BeowMvoHuSHbDllewXW6P3QecJDa_059rxqMXQHD3ts": "Botega LP TRUNK/qAR",
%   "zdEJQ8FLgwzzOXfZqO27A3Evf0gARZzcdtRrPk2z5RE": "Botega LP DUM/LLAMA",
%   "26-Lp6__tyesAeROgddjbKeE5lXhyRqbuvhpKM--dWs": "Botega LP wAR/MU",
%   "wDfCERaauacZovKJ6HjJ6F3T1LXl3w50nL410hXKz7M": "Botega LP GLX/qAR",
%   "Yu0lyyLnVX1Mxm-KMvvzjob-DW2g0dxho8kwM-0Tc54": "Botega LP HTT/TUSDA",
%   "pjSK_y-czOlCg1cpxlhoaCJRiFypMyb3KGnefIPqylk": "Botega LP PNTS/TTYES",
%   "lZv6UHZd1WsNK3jL5rSGcFiHzltpQPUVg9n5eMeJWP0": "Botega LP wAR/MAD",
%   "GeJXIQRNaSHE8hN2XzT8tmv4sMH6jCxY8EY0WwM33lg": "Botega LP RYB/MOODENG",
%   "PyUm93_h46JkCNb7Rgx4SCkRXe1WGZsWwrNtO6Yok-U": "Botega LP RYB/DUM",
%   "CVdwG8uAYwqaU4NoibM6grcbqv9rC4ffeifcEzBXoXE": "Botega LP wUSDC/RYB",
%   "t4aLLFC6Ms635bOY_xiCbDbiC4shDqeXjp45OHBJoIM": "Botega LP RYB/ARY",
%   "AaEXDrLjlYTfM8jaFtHQNuboh98Fz9YkO1OCIrBFBrE": "Botega LP EWV/RYB",
%   "WTgKCmo-ERjTyym5-4g_IVJMOrktcZZZullylT2KR3U": "Botega LP EWV/ARY",
%   "RoevgCpsRyjBuQLkXkSZpziwvieIqAirGOuUNbCZ1kQ": "Botega LP wAR/TKT",
%   "rrkUxaXBDORW4ptdLcJyARcxAY0jhPITxOjZgYG356Q": "Botega LP wAR/PEACE",
%   "AaijM5dtqNzldluY8gKk7O4zdp5UPv3u0cUEowga5ZI": "Botega LP AGENT/wUSDC",
%   "NX9PKbLVIyka3KPZghnEekw9FB2dfzbzVabpY-ZN1Dg": "Botega LP qAR/NAB",
%   "nzj48XewmWtr7bLqK8f2gS6CsXEK8CI028qyUdiL7Oc": "Botega LP BRO2/ST2",
%   "mMUl4kDwoMHtFcVzpVQHPHJ9ypy8O9ttKYUEoCUBdrg": "Botega LP wAR/SBT",
%   "28col0v_59eZwr2iWDZz0yGwGgr8V8sj-2CI455emXo": "Botega LP TTT/TUSDA",
%   "4IGzddk67tIZ9zS6AEGO4DA_hq5-LONepeWd3xqt5sM": "Botega LP wAR/dIO",
%   "Z1GxO3y6QdIwlGGIO0ai6v50A5vy1BXVT5zdrZWsEyk": "Botega LP DOGE2/wAR",
%   "A6u-wOhZZVdI6pnWb0hBhBYTU7j6JzZyTySFzaolFrE": "Botega LP AODOGE/wAR",
%   "SnNaD0ke3Dk8pSN-MkO0mtc66DqsAtCYHtniu0x070I": "Botega LP MAO/lll",
%   "_jQ--brkBoDuFA-e22ZuQq3KuGe5kGZutrtGmKPvGaM": "Botega LP lll/MAR",
%   "_w0rnbSDgDC1yC_5XkryIcsk9gSdpiMq4y0qLbpJjMQ": "Botega LP wAR/SXAO",
%   "ZeSJHyEbZqA3D3h5vMnFPZi3BY4LdgXxL_lZzIUcvPw": "Botega LP qAR/BELE",
%   "efWubq-YJ34ErYyslccr_MbWT-CdeDQVOwI6AoUpvAI": "Botega LP RIDE/MAR",
%   "HOJpKZTM8N1xDq3ScCswpiTz2ixOG2B7UGUS57xnYTQ": "Botega LP NAB/AO",
%   "ZV_FQiQ69XFAWTGesFmhiSEcD7EuZWVQ4WZjdAhgM0E": "Botega LP CALL/MAR",
%   "VUtLcz7fPpu_Gs3IDIeJYeKrz5T3aISGSvnBz82ZEgI": "Botega LP TRG/TUSDA",
%   "adYsOC2ZIXb8rp0e8QVSDUsgc2q5Q4WyTU04mZaeLVg": "Botega LP AGENT/MAR",
%   "4MYqWdc4_TcvVU0zoNMzuIZkUnazrSf0d-FsVjEPtSU": "Botega LP wAR/BELE",
%   "uI_tip34vI5UwwDPXrBO17rDGRGFMPpfMYbqb3MY6wI": "Botega LP TRUNK/CATS",
%   "QQpoLDH-iymHGH8ypS4eyMeG1_E_mIGcyTK_uh5oY9I": "Botega LP FFP/wAR",
%   "2jceFDX70udoqU9VmBvTvlnHLkX5XE5fJYJ3a3LE5tI": "Botega LP AO/FFP",
%   "zKNCEqN2eQOTLKeeutnsCS1-o_LqxxGLH0-diB2TMRU": "Botega LP ANT/MAR",
%   "PQuCL3DpY1YxtIJxM1n_Prgwg3n4QqqAx00y8V3sk0g": "Botega LP wAR/ARDINO",
%   "a4NEV4lfPM45ObWeiTY7tjzzJTTWGUrcBet_qFjMI7A": "Botega LP wAR/BAO",
%   "jdTAf0WNxRqOxTT9iUvaQSvCONo2VDmuaIdMTWyg-jY": "Botega LP wAR/ARIO",
%   "sC97cLJR5lpQNegTnTKBMR5yRLGtT-CKpLP_tjl4yDo": "Botega LP qAR/wAR",
%   "QZGGu7NQ9eJs-Iy4LuW6J7rTFM53J8CFh6ibe7K44S8": "Botega LP wAR/DOA",
%   "LyQLxkm4OaNZevDuJ13sHYSmIi9Oxa75zo3jASlaXZc": "Botega LP qAR/SBT",
%   "rFyagQAo0gzmopeWicjepUs3giawB_qou9A1Ys7JKIc": "Botega LP MAR/TKT",
%   "lCHuxmDwoDGQFBY4Zo3n8XOKSzZzy8PAtnfNAPEDRd8": "Botega LP NAB/DOA",
%   "cuF1-drHNo9UE181XgRa-uiP5syyTengZmGllZZp-tE": "Botega LP wAR/WDF",
%   "SjVwMy04UkkA0Yv-l5hI5De0N7pMRvS_tGtyyOqNNzc": "Botega LP MKR/MAR",
%   "sGtzsaci3BSoUMw5Ev-WQ-VUEk4rsmBr7qXz7q6w8Fs": "Botega LP wAR/CATS",
%   "Ky8sV4_Qae3fSLzFcJsg6ND8Keuu8tdsj6qYUYywfQs": "Botega LP wAR/SUAO",
%   "YWXxV1MPM3wYwziC9iuZIcR82UhSNxTgZbgykOUsvSo": "Botega LP STAR/MAR",
%   "nkEtNiUT2BKYgZfwFygTgctXY1LJu_K-OUPtSqDPHA0": "Botega LP wAR/SDAO",
%   "eAopfCQPdHEWTmbJ0SrnsEnbhO_ZwwMAK4MMVOezw1Q": "Botega LP AO/AOS",
%   "MZvo-FQq7p57ADa6TVM6p5eN7vdh1il5IbqzzjvVh7k": "Botega LP AO/Test_APUS_001",
%   "DMB9l4HSp-9qBvXtxDqDTnZZ3qFNCwJgivwSv0XEgN0": "Botega LP AO/MINT",
%   "1YJJ6X1vjgdyDs1ANzJoZ6w5pArf-sBTFC7v5vuOpYU": "Botega LP TEST/MPT",
%   "19XoP5GdBe3FhfQdmfVaL59BJJG5gotCuSkkrlkQ9po": "Botega LP MATRIX/MPT",
%   "57Taac-A-vry2l-_6iSNv268m6b9uTcxirUZgo2UIQ0": "Botega LP MCOFFES1/AOB",
%   "VcjUdMrJnCi9ZcBKVC0hK_0H--4ERnMyhQGYGp9fJt4": "Botega LP owUSDC/qAR",
%   "wL7LhayZHdjV8Cc-k-13zPGW50fobSo7r1xvH_gLytY": "Botega LP qAR/oqAR",
%   "pfRs0r3yAXR4Q9SuVMHjJadxyP9GseDRb_mkqCcIyBo": "Botega LP MPT2/MATRIX",
%   "jMla72tPc_qAaBOAqMQQ4dzQfg2-AI_CeUBJshdHBu4": "Botega LP qAR/ARIO",
%   "O3XNkMtPks7QfSWABss3UOVO0dC_xDrQzqqQvweZpxU": "Botega LP AO/DUMPET",
%   "uTVT617nw4Fq5dmeLxvraTg1PgryDF1jJUj4yPG70fk": "Botega LP wAR/MOMO",
%   "XIbSQ6D0sqHZ5MGv3qU3z6pMEA0vg-hfM5ofNABMauk": "Botega LP AGENT/AO",
%   "AVJF-g9es8nt9n5krkuIgPt0R5zxBTc5lNq-6JzT9Yg": "Botega LP wUSDT/wUSDC",
%   "HPkFE5qyaIxsxGYWvMXqnQ4fIZQHqJQ5LjFHVkBExEo": "Botega LP MOIL-S1/AOB",
%   "C5bbKU3B2yz-pJU8m3nV4LioCYzlCtm75MQR4FlOzUo": "Botega LP NAB/MINT",
%   "Ruw6Q5gVgZ-isWhRGW4LVvTu6rMost-J5SKsF4rF-rA": "Botega LP MINT/qAR",
%   "WjLpu62JJqXQIjXPBmCkoyWNvTJs4muOKcxdAieEmAY": "Botega LP MINT/qAR",
%   "UxLqhlBkIITPypglQOOrtcTQddFJU4utYFkwDqFbV7g": "Botega LP wAR/MINT",
%   "mqJsHpuJLk77PB0pVCv47KqT3U_xY_ZHQQwHvzUAsWY": "Botega LP AO/MINT",
%   "eKECsvAaDph0x7g8-mmrqp4skJEjBTCnykkft-HmikY": "Botega LP wUSDC/MINT",
%   "d2g6BtSbqayYg9--iVUSDyiEE49fC_sJVHGroqk7tOc": "Botega LP EXP/AO",
%   "5rROGt1CAvLDPuZ1MnWaMZfeEmieN1QjiYYC7zmOWB0": "Botega LP EXP/wAR",
%   "4-C-rMEh_C2_fDAqCEwEzPpcvDu_7YmsOMN2Rivm8-I": "Botega LP wAR/ARIO",
%   "VC55wQCvp3o4lcnzFSEEjE0Wg7ruFFaezBhsH0eGvis": "Botega LP wAR/ARIO",
%   "AaO68LmFURh6qECfpUuIyT2dfdalff79VC3NZAl_Buo": "Botega LP MEMEA/DNE",
%   "AnMWiOFl708rIndIdgRA8GaHIQqniWH2xEA2CEvHMN0": "Botega LP MEMEA/DNE",
%   "glE2s3yDgkA-YTIUiXOos-A-zDSkz_3ybZAybWLt53E": "Botega LP AGENT/BCAT",
%   "TH9KkYJzjemJOdQy38I0eTdhE9EPXuLMn8enqWOFyzQ": "Botega LP BEANS/AOB",
%   "ai47aFWWFnaM0isMFIIYlUZnk-uO7b4oyIvkqR8mcag": "Botega LP TEST/MPT",
%   "wGdMsMmqGVC4bN7gIsUiKG2Of0V6WctRhUQyz_i8UBE": "Botega LP TEST/MPT",
%   "_oWsi2p31w7iONYxBu1RInkNNK9ZSxlwVLdpEqE6s0s": "Botega LP TEST/MPT",
%   "Qm56nsWNDDssFW_8VS533ukuBAhmIVSR4A-9XQ1AMks": "Botega LP HUEL/EXP",
%   "KwYRym6Oku73keCTMrMUFxXRPuDTsv_NFPsvzUJdtH0": "Botega LP TEST/MPT",
%   "cmZi84AA3f717pna0ck-gM93wq1j1exRdNLdt7saG9o": "Botega LP wAR/MINT",
%   "xPr3l_F1OlvpVgeCtlTTkejNmR-28ZNWAcMMRbpUGdA": "Botega LP TEST/MATRIX",
%   "t4JkQ4i5rkYFsBI3_45pff4xkaQayoYYTvh7UPkrJs4": "Botega LP TEST/MATRIX",
%   "sf5fcxbpaB1WLbzRFhFYhRZTOk2toSyFDVxO7rxCIjw": "Botega LP TEST/MATRIX",
%   "HATiF_ca6ENn7aS5bcbkcZb7X5Nq_rhYJ7XE-YLYfuY": "Botega LP MPT2/TEST",
%   "lurwC6ZbuSQuPnamnGB-CgRx4OyuQqeNQq4EqoIZu1A": "Botega LP APUS/AO",
%   "b_bjXvUF0190O3Ony6YX4IfxZEy1weyZJdFRBBfoEg4": "Botega LP wAR/BOOST",
%   "GdB3ZOrcmHjzIrcIk59c-K_ZaTxFXmQ2vAORyAuX3s4": "Botega LP AO/TKTXX"
% }

-export([run/0, run/1]).

run() ->
    run(prod_basic).

run(GroupName) ->
    try
        Wallet = ar_wallet:new(),
        
        % Convert JSON comment to Erlang map
        ProcessIds = #{
            <<"S5iWl7L76dBpvrm-i_SRWODchdLYBGwYeSrIxC8v-vI">> => 
                <<"Botega LP ARIO/wAR">>,
            <<"8sCebZHN8C5eheRWF243TFCF84OLqGWBYP8PdKf8NK8">> => 
                <<"Botega LP AO/ARIO">>,
            <<"Lji25RS3-9LXZe4-gFpdnBMgqyRDqUzZPHWS-HqPWsU">> => 
                <<"Botega LP FOREST/MAR">>,
            <<"cG2uakimBR65Yk2YxPqBYfnONVosuPXD5_8j4lNa2Fs">> => 
                <<"Botega LP AO/ELEPHAS">>,
            <<"7zFWm1xqJ1u1slsvOcNbeyLCzxozOmyVJeRJjOH_i4I">> => 
                <<"Botega LP HUEL/AO">>,
            <<"avosk_bDmNBOq0H4XgQ7aCDvcndI1OCglWc1OIAq9hI">> => 
                <<"Botega LP PORT/MAR">>,
            <<"caLDjssasuUw389rjc-kJGsU-bEFgnGTBBTJoFRNBvM">> => 
                <<"Botega LP DUMDUM/wUSDC">>,
            <<"ttOZLNyBZokWYmAlNIDqngzYXj9sEIU22B0sBTJobWc">> => 
                <<"Botega LP BLAH/wAR">>,
            <<"wLtGns2uIfcwTDm6QXeiL5QQa3tQPiI-Aztr5aqdyiA">> => 
                <<"Botega LP BLAH/wAR">>,
            <<"20cEmow3vtkcmGbqgDIq7J9iLJB46_ku_LUoLvQT3uY">> => 
                <<"Botega LP WYOG/AODOGE">>,
            <<"0yR7PW8sCb_SuOgf0s8t1xX0-X2pqLnm6fEF776jvgo">> => 
                <<"Botega LP wAR/ARIO">>
        },
        
        % Process each pool
        Results = process_pools(ProcessIds, Wallet, GroupName),
        
        % Check if any results contain errors
        HasErrors = lists:any(fun(Result) ->
            case Result of
                {error, _} -> true;
                {_, _, {error, _}} -> true;
                _ -> false
            end
        end, Results),
        
        % Report combined results
        case HasErrors of
            true ->
                loadhb_report:report({flow_pools_eval, {error, Results}});
            false ->
                loadhb_report:report({flow_pools_eval, {ok, Results}})
        end
    catch
        Error:Reason:_Stacktrace ->
            ErrorMsg = {Error, Reason},
            loadhb_report:report({flow_pools_eval, 
                                 {error, [ErrorMsg]}})
    end,
    
    ok.

process_pools(ProcessIds, Wallet, GroupName) ->
    ParentPid = self(),
    
    % Spawn a process for each pool
    Pids = maps:fold(fun(ProcessId, Description, Acc) ->
        Pid = spawn(fun() ->
            Result = process_single_pool(ProcessId, Description, 
                                       Wallet, GroupName),
            ParentPid ! {pool_result, Result}
        end),
        [Pid | Acc]
    end, [], ProcessIds),
    
    % Collect results from all spawned processes
    collect_results(length(Pids), []).

collect_results(0, Results) ->
    Results;
collect_results(Count, Results) ->
    receive
        {pool_result, Result} ->
            collect_results(Count - 1, [Result | Results])
    after 30000 ->  % 30 second timeout
        [{error, <<"Timeout waiting for pool results">>} | Results]
    end.

process_single_pool(ProcessId, Description, Wallet, GroupName) ->
    % Step 1: Hydrate using cron
    CronPath = <<"/", ProcessId/binary, "~process@1.0/now">>,
    Path = <<"/~cron@1.0/once?cron-path=", CronPath/binary>>,
    
    Message = #{
        <<"path">> => Path,
        <<"method">> => <<"POST">>,
        <<"target">> => ProcessId,
        <<"accept-bundle">> => <<"true">>,
        <<"accept-codec">> => <<"httpsig@1.0">>,
        <<"signingFormat">> => <<"ANS-104">>
    },
    
    {ok, Url} = loadhb_groups:get_url(GroupName, compute),
    
    Result = hb_client:send_message(
        Url, 
        Message, 
        Wallet, 
        #{}
    ),
    
    CronResult = handle_cron_result(Result),
    
    case CronResult of 
        {ok, CronMessages} ->
            % Step 2: Monitor hydration status
            MonitorResult = start_monitoring(ProcessId, Wallet, 
                                           CronMessages, GroupName),
            {ProcessId, Description, MonitorResult};
        {error, CronMessages} ->
            {ProcessId, Description, {error, CronMessages}}
    end.

handle_cron_result({ok, #{status := 200, body := _Body}}) ->
    {ok, [
        <<"Successfully built a cron request">>,
        <<"Successfully evaluated a cron request">>
    ]};
handle_cron_result({ok, #{status := Status, body := Body}}) ->
    StatusBin = integer_to_binary(Status),
    {error, [
        <<"Error: status=", StatusBin/binary>>,
        <<"Error: body=", Body/binary>>
    ]};
handle_cron_result({error, Reason}) ->
    {error, [Reason]};
handle_cron_result(Other) ->
    {error, [{unexpected_result, Other}]}.

start_monitoring(ProcessId, Wallet, CronReqMessages, GroupName) ->
    Path = <<"/", ProcessId/binary, 
             "~process@1.0/compute/at-slot">>,
          
    Message = #{
        <<"path">> => Path,
        <<"method">> => <<"POST">>,
        <<"target">> => ProcessId,
        <<"accept-bundle">> => <<"true">>,
        <<"accept-codec">> => <<"httpsig@1.0">>,
        <<"signingFormat">> => <<"ANS-104">>
    },
    
    monitoring_loop(Message, Wallet, CronReqMessages, 1, 
                   undefined, [], GroupName).

monitoring_loop(_Message, _Wallet, CronReqMessages, 4, 
               _PrevBody, IterationMessages, _GroupName) ->
    AllMessages = CronReqMessages ++ IterationMessages,
    {ok, AllMessages};

monitoring_loop(Message, Wallet, CronReqMessages, Iteration, 
               PrevBody, IterationMessages, GroupName) ->
    {ok, Url} = loadhb_groups:get_url(GroupName, compute),
    
    Result = hb_client:send_message(
        Url, 
        Message, 
        Wallet, 
        #{}
    ),
    
    case Result of 
        {ok, #{status := 200, body := Body}} ->
            case check_body_progress(Body, PrevBody) of
                {continue, NewIterationMessages} ->
                    timer:sleep(1000),
                    monitoring_loop(
                      Message, 
                      Wallet, 
                      CronReqMessages, 
                      Iteration + 1, 
                      Body, 
                      IterationMessages ++ NewIterationMessages,
                      GroupName
                    );
                {stop_success, NewIterationMessages} ->
                    AllMessages = CronReqMessages ++ 
                                 IterationMessages ++ 
                                 NewIterationMessages,
                    {ok, AllMessages}
            end;
        {ok, #{status := Status, body := Body}} ->
            IterBin = integer_to_binary(Iteration),
            StatusBin = integer_to_binary(Status),
            ErrorMsg = <<"Error: iteration ", IterBin/binary, 
                         " status=", StatusBin/binary, 
                         " body=", Body/binary>>,
            AllMessages = CronReqMessages ++ IterationMessages ++ 
                         [ErrorMsg],
            {error, AllMessages};
        {error, Reason} ->
            IterBin = integer_to_binary(Iteration),
            ReasonBin = list_to_binary(io_lib:format("~p", 
                                                    [Reason])),
            ErrorMsg = <<"Error: iteration ", IterBin/binary, 
                         " reason=", ReasonBin/binary>>,
            AllMessages = CronReqMessages ++ IterationMessages ++ 
                         [ErrorMsg],
            {error, AllMessages};
        Other ->
            IterBin = integer_to_binary(Iteration),
            OtherBin = list_to_binary(io_lib:format("~p", 
                                                   [Other])),
            ErrorMsg = <<"Error: iteration ", IterBin/binary, 
                         " unexpected result=", OtherBin/binary>>,
            AllMessages = CronReqMessages ++ IterationMessages ++ 
                         [ErrorMsg],
            {error, AllMessages}
    end.

check_body_progress(Body, undefined) ->
    {continue, [<<"Iteration 1: body=", Body/binary>>]};

check_body_progress(Body, PrevBody) ->
    try
        BodyNum = binary_to_integer(Body),
        PrevBodyNum = binary_to_integer(PrevBody),
        if 
            BodyNum > PrevBodyNum ->
                Msg = <<"Body increased from ", 
                        PrevBody/binary, 
                        " to ", Body/binary>>,
                {continue, [Msg]};
            true ->
                {continue, [<<"Body unchanged: ", 
                           Body/binary>>]}
        end
    catch
        _:_ ->
            {continue, [<<"Body (non-numeric): ", 
                       Body/binary>>]}
    end.