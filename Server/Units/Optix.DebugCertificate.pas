{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                             Project: Optix Gate                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://bsky.app/profile/darkcodersc.bsky.social           }
{                   https://github.com/darkcodersc                             }
{                   License: GPL v3                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{  Disclaimer:                                                                 }
{  -----------                                                                 }
{    We are doing our best to prepare the content of this app and/or code.     }
{    However, The author cannot warranty the expressions and suggestions       }
{    of the contents, as well as its accuracy. In addition, to the extent      }
{    permitted by the law, author shall not be responsible for any losses      }
{    and/or damages due to the usage of the information on our app and/or      }
{    code.                                                                     }
{                                                                              }
{    By using our app and/or code, you hereby consent to our disclaimer        }
{    and agree to its terms.                                                   }
{                                                                              }
{    Any links contained in our app may lead to external sites are provided    }
{    for convenience only.                                                     }
{    Any information or statements that appeared in these sites or app or      }
{    files are not sponsored, endorsed, or otherwise approved by the author.   }
{    For these external sites, the author cannot be held liable for the        }
{    availability of, or the content located on or through it.                 }
{    Plus, any losses or damages occurred from using these contents or the     }
{    internet generally.                                                       }
{                                                                              }
{                                                                              }
{                                                                              }
{******************************************************************************}

unit Optix.DebugCertificate;

interface

const DEBUG_CERTIFICATE_PRIVATE_KEY =
'-----BEGIN PRIVATE KEY-----'                                      + #13#10 +
'MIIJQgIBADANBgkqhkiG9w0BAQEFAASCCSwwggkoAgEAAoICAQDApxaCK/oNuYXV' + #13#10 +
'HU9wnkbT00+hSjxLRqONTYDdiQmvIhDNv4MEa3T4/pyZJKM7oTGT6luiMywO1zI3' + #13#10 +
'/rLkDxFMML11EaVbg6rKEawTn9YE06tGWb6RsjfAZRfhALHqwqLEfNnMztw3b33d' + #13#10 +
'nma7o13BVwcM6HP8361O16fgrkv8OjnMof7PuO4YJ4XVOws1uaq+yUpyroNSHSOu' + #13#10 +
'/pFB+s1k+FuCjTzzWuHGLJba64+3x4WlBeIKAh+TWrNKA2FuYmDb6gkJfNPqrbAU' + #13#10 +
'Bi7uOGszOAjP6PNFh/GORtJ7hkQQOxGF21NkvXKcOsPbljrGnSSVU3HVZ6MR5tg3' + #13#10 +
'nEytHtXPMvbjth5q/F8f31P2yPPRCK9vFkWsWSJJWIFb7zgsmknBKh+CYlkjboK7' + #13#10 +
'kEn8IMtbGjPyM8oH+NivjNRYYhH1m/0F8kB2OPtK/NnYUWwbt26KDWjxznv2I4bi' + #13#10 +
'w5imbgU/j8cMTbaBII+t84DtTURaFHDp4rICEX1/G/gw+EvWN2WQe0CPP5bAE78f' + #13#10 +
'AyEoGoN0jM9sj1zzDBAp/tuLYHaFvIiyxHpvYlFl2bMhy0AcL0aBNnrkmiFspAkY' + #13#10 +
'FOUUelVSjWgDkYLbw8fYaHpz/wstTTIkx3xlrx8oS+ODHZkShRb/8S7lMwpdsWim' + #13#10 +
'7okGESQuRDbBzwD+Y1kV3oYuz87nFwIDAQABAoICADiKUT7O7UpN4otTWNcyfJL6' + #13#10 +
'd2pcwSlOh/2We3WJHIB7vPbFeAw6BVB2WTtQ119OxaAlrdMCtbF9Us3AQHxSN79m' + #13#10 +
'ppPPI/qhv+QiH6B6znMINEInYJid92LNyJ5Od+jwSpjkE3/aTiMu43/BV2FGZuOx' + #13#10 +
'SO5u4muaseBrxmdYgBB8fi4idQg9vBL2tn6IH5ga75Oiu+jAcuYMi0Z2i+cTVmiF' + #13#10 +
'w0Wjcfnx+EUTHQsPC1Tih8SQ33AUg3PZsLjOGTAuS0L/grt4GTHM29pkni1n/X5A' + #13#10 +
'ucVbFtqXNTuOaYO7OgqRa2kfn2/3ccWMMaMmigV0ZSI2JTh+fAtq9pYLly1P2Wu8' + #13#10 +
'YP51sf6oKptT4mQ6LANmLNoUU4ptFMRPDfW97KUSdt1a0U1Z7oX4udLxvjxxH863' + #13#10 +
'Y+4rot7vPsdolCmWPywZbpsXngNMPQg6VqzAQCiWKrRLGdtOJ66cczcJ+7SKZTsr' + #13#10 +
'LnxneWeFmXjrTOLTYRJm36tJYnJmKu9U5M6IuzzXw4b5r5phNh5D0LhSBVCKgnt0' + #13#10 +
'sEef/IT7TZgoUZZokshuA3AlAJ8TdcVIrCyt33W/7GckGehiXYCu9qo6E+QBW2iI' + #13#10 +
'/5iEedK94+61awozgbeEkC0jk3DlTgzjVrYinl1x+CK/bE9PrAKWKS3aOWPXAeyH' + #13#10 +
'hf43v5GUkbm7dKX/YSChAoIBAQD/qrmVkS9fad6YbXwdK+FmStPuXnspbNfKiJdw' + #13#10 +
'+Pn69KlP3pnNIg5+8n1OLqSg78zoiKPr+y/vl2oNY1jqchuMIEvQPa+k5ME+Cu+I' + #13#10 +
'9a3K2FcyqKOlV8tCrfF1XuSII6uOCvTZ6mO+aS8yQ/nIW6iK7ks6maBbh5c7fQx/' + #13#10 +
'78h0Ey4pCmJbWo6peovUebqA6eB4NzxfosyStaGyOHnKCNU/2mKNGMzKIhOXrawO' + #13#10 +
'mVs2hO0dNaEalBCBuBSDyTANWNEBkW+DQl5opZoiEvcpv+2cAsMDDkK39gzHOjCQ' + #13#10 +
'VFnLA39CY4YbsomyR33nx662xm0gyhIG5p/LO4s55YQujCMvAoIBAQDA51hh+fgM' + #13#10 +
'9MhWgMqFqJxQDjIZevlG7oYiQ/vWMVDSH5BPZbiZGnDBrJIdkbCTr5UE8JNdK2tE' + #13#10 +
'aEl5AVOozqTyrYYP5RUuvjwhlM+dg2HIBVsNdZsC2WjfmT+187bnG9PVvLxqAP6Z' + #13#10 +
'DaAm7WJAvNqjb8wwSMdjop6zu1RrBreb3eMd7FT4PxMtHBW27JrUM9bMzh1++LhM' + #13#10 +
'VRUk+i9K+Q5PbfH6MxOX4XnV0fWyRJYEGBPrQhAXoYzXccRIRr+lxgs+X6YTfc4L' + #13#10 +
'KOPiPQIHr5Bs6Poq2rzsQraBVty3UhVwL5p01GK9AWQcyWwP8vWMLKyYb+JqWhQ3' + #13#10 +
'p8ykzxJW2CCZAoIBAQDlCrWn4KFoARiBxdPi7mTHl1G1wr7jpbbMowHJG8QXpwfn' + #13#10 +
'nax7sX77C5JmcDLcWvhfecogpq+THTrNM6t8nS1Ao85HiHvKZYUZaAKf2DkR+C5m' + #13#10 +
'G94/Sh+2ZM5kL9bcf9g5MGeasfkZpHG7vloPvM2JaWHL5cVbOmWpzaVdGBukoWpB' + #13#10 +
'KiRjYwVbn4WVnFgRFXUbPaDArMBIzp9FYoL5FGC3C5YugYE1tMGIqPXzPkAQ35Tt' + #13#10 +
'DhZ+EReYnqkCR0LdMixME1araNHbPkCPNh5nMKJVmcPLAQOUesGH2gGXy3mF/mQL' + #13#10 +
'vc1vEst5A8Mv2hwTckBhl6X+uK1/DKD2BUAd0z3RAoIBAHXQVWBxZJe1IG4Edqwv' + #13#10 +
'EFisctixDHHP18gxsSteLzhF4sM7IwUO6vK+TPcWNbvdLqw3qijrDbZX4xeuQcQF' + #13#10 +
'gSRd6lTm2osT0HHKCwruZNfevX945lVVJwH4/LpJwNlhW31cqc4sEVK0ya08qhdT' + #13#10 +
'wEy1SQXsTSqC9V5b+PDsx1LAr6dk4XhEPZf9YR25X85eYr/u2oDjstub0zAkRKIL' + #13#10 +
'fjvwsCrY6wyFvv1iICbiUKTrd67lSCLGndgvOWvTGdVLIS7VB/87hI24SYWxMP40' + #13#10 +
'sYZyu+dUdLHvBLv1qxOjZEiCKllUJYG9ycDzG7aZ8nHMDxWCr8u4fESJjSjS9yZn' + #13#10 +
'FXECggEARAuSMrVO8TwxfJ+wuAEPouZRKGIWgl7xPozrp0FIy5FcF6v9TjMnoXmi' + #13#10 +
'dGMMsqxwWB8TtaEaFarIHozvokKUN9ea8KBqNqaEf3soig+akPDRuRGKCUywQKbG' + #13#10 +
'hPMOsFWUExcqZaqnX409hp7DGt6IdkxtqZ8yZP7gDlB/cc2LTqKMeK3RWYVnkLTb' + #13#10 +
'6zJZtD7RoCwNUoRLzKWUgH4LhXy3s228BTy5KKBVEj7ZxlfwSKoWGmdaYHk/g5Xy' + #13#10 +
'mvo75+fNz/yxt25dNRzOcYb707hF5U843lhg+KyPfcDo8vMJnPh65Z2WRToLwsdG' + #13#10 +
'OHHejiGEvpDQtxLP3WbvAl/tA/JA8Q=='                                 + #13#10 +
'-----END PRIVATE KEY-----';

const DEBUG_CERTIFICATE_PUBLIC_KEY =
'-----BEGIN CERTIFICATE-----'                                      + #13#10 +
'MIIE4jCCAsoCAQEwDQYJKoZIhvcNAQENBQAwNzELMAkGA1UEBhMCRlIxFDASBgNV' + #13#10 +
'BAoMC0RhcmtDb2RlclNjMRIwEAYDVQQDDAlsb2NhbGhvc3QwHhcNMjUwODExMTMx' + #13#10 +
'ODM5WhcNMjYwODExMTMxODM5WjA3MQswCQYDVQQGEwJGUjEUMBIGA1UECgwLRGFy' + #13#10 +
'a0NvZGVyU2MxEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQAD' + #13#10 +
'ggIPADCCAgoCggIBAMCnFoIr+g25hdUdT3CeRtPTT6FKPEtGo41NgN2JCa8iEM2/' + #13#10 +
'gwRrdPj+nJkkozuhMZPqW6IzLA7XMjf+suQPEUwwvXURpVuDqsoRrBOf1gTTq0ZZ' + #13#10 +
'vpGyN8BlF+EAserCosR82czO3Ddvfd2eZrujXcFXBwzoc/zfrU7Xp+CuS/w6Ocyh' + #13#10 +
'/s+47hgnhdU7CzW5qr7JSnKug1IdI67+kUH6zWT4W4KNPPNa4cYsltrrj7fHhaUF' + #13#10 +
'4goCH5Nas0oDYW5iYNvqCQl80+qtsBQGLu44azM4CM/o80WH8Y5G0nuGRBA7EYXb' + #13#10 +
'U2S9cpw6w9uWOsadJJVTcdVnoxHm2DecTK0e1c8y9uO2Hmr8Xx/fU/bI89EIr28W' + #13#10 +
'RaxZIklYgVvvOCyaScEqH4JiWSNugruQSfwgy1saM/Izygf42K+M1FhiEfWb/QXy' + #13#10 +
'QHY4+0r82dhRbBu3booNaPHOe/YjhuLDmKZuBT+PxwxNtoEgj63zgO1NRFoUcOni' + #13#10 +
'sgIRfX8b+DD4S9Y3ZZB7QI8/lsATvx8DISgag3SMz2yPXPMMECn+24tgdoW8iLLE' + #13#10 +
'em9iUWXZsyHLQBwvRoE2euSaIWykCRgU5RR6VVKNaAORgtvDx9hoenP/Cy1NMiTH' + #13#10 +
'fGWvHyhL44MdmRKFFv/xLuUzCl2xaKbuiQYRJC5ENsHPAP5jWRXehi7PzucXAgMB' + #13#10 +
'AAEwDQYJKoZIhvcNAQENBQADggIBAChfoUrqPrSZz4DeVVdarssnmmzYUREP3fRV' + #13#10 +
'Qe08Ch8BV543KcUfy63+j1zxVSurqoSStBm55UjA8FxD2OHI52nImi8For2BEvjt' + #13#10 +
'/7ujvrMmtFnRmMFi0ia0xkg2gAXUH01/1GEpCjHUBT476iJ13k6CC/luGQqpXA0u' + #13#10 +
'ThtIKbo0cAPa+rCG/ssQD6ADtNKhBOuGJumuJvSGqle+IQB4MYT7wrSel12sVQRk' + #13#10 +
'yuv87yapr06ClmzdOS1MYyijN5FvjK36361ABlHhVpJCVM1Apjn2pRGHjEszH3EN' + #13#10 +
'7nbABWgTSwctQtd12lEL12qvymKhCJPkIcOkY/S58h7UfnP1v44IJmUV2/edl8fe' + #13#10 +
'YmwNTHBrWNADBPjnBkArR9ZkYAHMBmNvvrmIFWdzbuepPHzSi/AvueZ5PqsGWvBh' + #13#10 +
'd7gMSCwWkQryrckPhFKZNRiCr8sNZ+uIiv0VUR1+bGSsAEXII3NJXlO6JFOVlMcZ' + #13#10 +
'WK4Bsr0NwDqNxNLnnNR+jq6NMam1XFgqk14TD/Np50rSxDjZ4Ewy+eOI8C5N6C2R' + #13#10 +
'HtshlBEECf1j+uRbUbflQQH3PsxzaNLvb4nOsC1fst8lXzd07vFV/JiD9pu8Xsft' + #13#10 +
'Bjcvn78dpjLMRar+eixn7cRkmnDFUXrfo1E/hhFSqnmJ1RtNg26Rz0KusOr/aX0a' + #13#10 +
'ZH5witlG'                                                         + #13#10 +
'-----END CERTIFICATE-----';

DEBUG_PEER_CERTIFICATE_FINGERPRINT = '92:3F:50:8D:CC:CB:13:AF:47:70:62:60:11:BB:0C:23:D0:E3:DD:C9:32:C5:BD:47:3E:' +
                                     '85:F0:89:7C:C1:A4:9D:C4:03:94:28:8E:50:41:69:55:19:B7:D0:7A:71:EE:FC:DA:C5:' +
                                     '54:2F:A2:E5:0D:86:3E:7E:87:10:5A:16:09:AD';

implementation

end.
