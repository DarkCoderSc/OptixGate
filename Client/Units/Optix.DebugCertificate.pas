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
'MIIJQwIBADANBgkqhkiG9w0BAQEFAASCCS0wggkpAgEAAoICAQDRfe5Ni6xCxE8r' + #13#10 +
'ABUh8rbeXZKyXMBIi6jaAHx+bs/l1M54A2x3UKyrxepW6ZOot9EpEXy38U8Bgat8' + #13#10 +
'EyFmem6rMzxprgRGIjlIgQrXCLwxDXAPN1WvFuQBYMClf/KOi/mpRNfj8I2Hk2Y8' + #13#10 +
'QvQ4L/goknYfjvYgeYPzWsoNmgAFz63fRYiFFDSQYl2x4p78ZcO2I4rQgkI9hhat' + #13#10 +
'dv5iUz08lz/4LoctUoo5TYuRryEBGOYajYUjac6cxC5HBbhf4jxU8CechTfDvS6h' + #13#10 +
'hxmbuultTvhtexONSD9+KeFQUj1Dn0TwCwwdSjTxwOaH7ZQad+WyUDlXKiNJFteu' + #13#10 +
'GczfUddvVIJlC1OqGagsnz1GGuCWlZRiawCKbt8CAmo38oEtyqc2meTJHD5LM4m5' + #13#10 +
'n0Xbqf4Ak0LHP+eF5Rpwgro92KKTNGyONQA6kB4KcyMqqgL5S1HlvJE/MxqjBZMO' + #13#10 +
'FLBOUIjEHF67AfMVDvLFI7UNUWsexsdCDwj9UUloA7S3u68WHo9cYLn54TfX1gF1' + #13#10 +
'349STMQxuW5xUGRDTtqGkKeF9bS2g96jA2JUO8gFjKotB1RY8Hd5zlQaO9+BKWKu' + #13#10 +
'6PQMeosImPclU45AJIsxmA17N5Q5wJVvpQeShgP3F8nkUPiBfqK//h8+GLXMybTy' + #13#10 +
'TpWboP4dYo0l1RZ8XCA0MYiChW7PXwIDAQABAoICAH4lLWTGcSD3IpDLCO6bP2Bg' + #13#10 +
'yh/a1/IXHsK+vjhHs6o6XWVI8nxaOM9qarSRC9fxb+Ih5I7CoIdQtJkOz/LsUE2I' + #13#10 +
'mZ0tKuesXf/5rDRVzzReWhbfhBndV8g4oWhGxiDtypURnkpkJGT57frlejeHMxAD' + #13#10 +
'OVvBcxWU9k6IYdiU3eSh7JngPdeclhZHFRIzWvaytImtlz384bQ4VNl5KK6+XsZo' + #13#10 +
'cR2Zrs4swIzG0/8SjHYjrxNYGk57vO0K+K7Nk/swuJGQ1tlaIn0cIsMjHdl3UG4A' + #13#10 +
'unj6Poi5c3od7820560bb6B3loDgmUblmCtLdP4HKmoYAp8pAXxWY+MzJ/llR6pr' + #13#10 +
'4WHh+umdGXYQxjEdhfS/sEgw+PZVPMOEmcSpfAWfLsuOSD3r3+0KPjQnRMGPFpZk' + #13#10 +
'+C5WL0CBN2lYRWxHoryr4xn/tUcaycvqqMgX3c+rHasntJUXqgubbw4GzhQW/oEk' + #13#10 +
'fZD08ZoDLbS9F/xRwAjbxivLKZLmjUllTJEUWc9Z8emc5EShy+LX9e2d4K/DW7F/' + #13#10 +
'SLwp1i8AyLueC8i6rPnAi1mEcPt8d7SCvqy0ubxKFCdEVIUqNjl4Ccb3VXN7t6xj' + #13#10 +
'1lKHNLNwXkUUX4jibzo0PXNlvFWT7sgGn8N2oElQ1SvP1wQPioPBreK8ZbiKwyt4' + #13#10 +
'OYNUkYBgu3q4RKCVHvDxAoIBAQDzkKT4JH6uKNKfYr+Bc8MwBTmAxTh/6/nplcB4' + #13#10 +
'g7ZhKLb3KwkruQLV9g6tJYN0YfMdHXsldFcLiHYYewFAW38jUC5/yewlnDiFdwn9' + #13#10 +
'hvNjI4leRY4zVS3pM5azNL3QlimwnEC/MQnnIWI8CV7vqb4GOB+r1N0uatMXi7ll' + #13#10 +
'Sq+zjRw/OlC0TSQFZKyGRyrAZQOvk+NK+4pCNlMdoDPa95RydPdOk9NsjxG8vRpi' + #13#10 +
'I+aV1pIx3eJ2S8aiYoP63HHsbbIuNIivvBSb0wTw2KnsjasO5WWae+N4sZVLfcH+' + #13#10 +
'h9TxDdm0nm9vxT3yDmiSsl0MGim6SuBfladUlT2TODhVxJHZAoIBAQDcL/Te+jzT' + #13#10 +
'HyqabSJ8ZRkOsL6IExd1HVyJfUdNQC3SO2KFZhWeyMgJbmPvYFw7f7hn2dS7ZbRx' + #13#10 +
'JEB9mXmbA90ZAdEDOVzqDOe3MHPwFkhXelejBzBlv1nyP3m64xYK5AnvHLXIcmA8' + #13#10 +
'MhiuVUHrYG5RY8IKAp9E9TjVn81Vo/FrH7RrcHVMO600nm/d+tMAdjdn7znpjR95' + #13#10 +
'Z9lWrxQLxL1aQxrF6OzwmOjYmsZSczYzUFvZpj678pxhUMhgocykDzU+x5QgHLPY' + #13#10 +
'yPQggSFHcasG05JE08/MENswNNhISHEFL/tyq9zbRpgkw8/Cnkl/CYyowINRwQWx' + #13#10 +
'r+eGS9I3MW/3AoIBAGO+7cBtea+rKi55y0GhfeLxkM4Kdov0wMEHQe5YylzZxqxM' + #13#10 +
'ZJIST3X+MZ55CW+lGWHoC+GX1nnzpaF65lVL4zI8CP7uW5Y/duo5iM7Z8Fy+VehU' + #13#10 +
'hdrB3G5vYRz80WtVw7b/Foj19T9BbbB2ifmOQzLa0yWUpAv5VX+ECiXQQ0o91L6U' + #13#10 +
'DC76lHDc/MZ06K4n8C18XA3+G23oP3uXewUGdTW2bv0wDtqbcMEhlGdymmDsoaxo' + #13#10 +
'WPDnMW6Y98HJe6I+fkTr4LpPRQVXkuNrZ5NnpRERot10NQDdgO2Mj+DEEdGyQWD1' + #13#10 +
'rV9RoH+7pNnGz9I/+KzBM0gRypgaEkBzOqbtPGkCggEBAM/FDOqO9HA/e4MGskDx' + #13#10 +
'YOikzNU4Nq1TTGuMZIi3o8U6NigsgQA489E6cmCB6+ZVCZwtR3ihathMXfJmrQIi' + #13#10 +
'bjJXrXO+YDeWf9wzg2Msk4zALAvxD7EjE+vsC6DXAYi7T8hqrfn8O2Tx3Ko8VxyU' + #13#10 +
'/OQeHw41FTcLslqaFTbXoUn1O9kZ4AVUVodNbb/oG8zWwEd5zIremLGF1ebfBCpq' + #13#10 +
'tkaDBbN0rIgHscganENW5fEbxeAl6BKPxxJpGPteEe1JUHPx/Ysjqm1IilxVnSJ+' + #13#10 +
'VuT1JZN+zxj/WuUtKF1kxgkgEsRM+fWLsqsWfmhl1nJif3EJX6D4Y7gchXsK9s0Q' + #13#10 +
'AFcCggEBALvLbUOcSnJZIBw44cWEJKEEkIrfE+U4xqkNuFjcc9zpxybMyA1Ep0la' + #13#10 +
'wJxvo9PfNTMz0xv1lc9E6ugxce7gnyDJxAQ9+NLiZRkr3d6HEYbfGzAHneFsxaxT' + #13#10 +
'i8t3i8yzFWjiE9s51HZnFtkVGoZ42SETS/LgTAbmBTRBbWbGrN/imjDD04yVnCEN' + #13#10 +
'w4NPdUEa5ZjbSXrGav+zCmPn0sJgjjuejj3hKk4Y/xg0EIirCKvCiZRDFDLdkxml' + #13#10 +
'ZTWf+AKoRWxzsrJEZemb/FOJL7sTWk3UL1oy7VU1OPorsSTBlK5mMwCHjBOrPSUv' + #13#10 +
'kVItNONqOMOmQIUCoTJZNjcEpNv3wHM='                                 + #13#10 +
'-----END PRIVATE KEY-----';

const DEBUG_CERTIFICATE_PUBLIC_KEY =
'-----BEGIN CERTIFICATE-----'                                      + #13#10 +
'MIIE2DCCAsACAQEwDQYJKoZIhvcNAQENBQAwMjELMAkGA1UEBhMCRlIxDzANBgNV' + #13#10 +
'BAoMBkNsaWVudDESMBAGA1UEAwwJbG9jYWxob3N0MB4XDTI1MDgxMzA3MDIwNVoX' + #13#10 +
'DTI2MDgxMzA3MDIwNVowMjELMAkGA1UEBhMCRlIxDzANBgNVBAoMBkNsaWVudDES' + #13#10 +
'MBAGA1UEAwwJbG9jYWxob3N0MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKC' + #13#10 +
'AgEA0X3uTYusQsRPKwAVIfK23l2SslzASIuo2gB8fm7P5dTOeANsd1Csq8XqVumT' + #13#10 +
'qLfRKRF8t/FPAYGrfBMhZnpuqzM8aa4ERiI5SIEK1wi8MQ1wDzdVrxbkAWDApX/y' + #13#10 +
'jov5qUTX4/CNh5NmPEL0OC/4KJJ2H472IHmD81rKDZoABc+t30WIhRQ0kGJdseKe' + #13#10 +
'/GXDtiOK0IJCPYYWrXb+YlM9PJc/+C6HLVKKOU2Lka8hARjmGo2FI2nOnMQuRwW4' + #13#10 +
'X+I8VPAnnIU3w70uoYcZm7rpbU74bXsTjUg/finhUFI9Q59E8AsMHUo08cDmh+2U' + #13#10 +
'GnflslA5VyojSRbXrhnM31HXb1SCZQtTqhmoLJ89RhrglpWUYmsAim7fAgJqN/KB' + #13#10 +
'LcqnNpnkyRw+SzOJuZ9F26n+AJNCxz/nheUacIK6PdiikzRsjjUAOpAeCnMjKqoC' + #13#10 +
'+UtR5byRPzMaowWTDhSwTlCIxBxeuwHzFQ7yxSO1DVFrHsbHQg8I/VFJaAO0t7uv' + #13#10 +
'Fh6PXGC5+eE319YBdd+PUkzEMblucVBkQ07ahpCnhfW0toPeowNiVDvIBYyqLQdU' + #13#10 +
'WPB3ec5UGjvfgSliruj0DHqLCJj3JVOOQCSLMZgNezeUOcCVb6UHkoYD9xfJ5FD4' + #13#10 +
'gX6iv/4fPhi1zMm08k6Vm6D+HWKNJdUWfFwgNDGIgoVuz18CAwEAATANBgkqhkiG' + #13#10 +
'9w0BAQ0FAAOCAgEAz4wdWg8SH/drd4u1RkBcBVB+z5nPUIT8Em6hX9Opd+7h5jGr' + #13#10 +
'n+JoEcNwdEZ7bVmSDq/BnBQgxiBZvpBQyM0QwGtdAd3wnJXA0Dr6FPMkhBj5dk5X' + #13#10 +
'7am5V6z9avRnMrJyTsqF4dAe8nqj+WmIPiJ7CRqpFi5xMpv3Abm9oIkX4fl7dJi3' + #13#10 +
'd9B6WCXWGZKTxIpKDMc+yYfGFU8PCbKRr8bmWTZ0SOue6Lxz++pmBfpeZh0HOVWM' + #13#10 +
'q/uF6XKFdKgJy33KToITqhB5EfDlICa8znQbYOqnZP/OD6l9zmRq1XNx2bMkp+4l' + #13#10 +
'j/WEA9ycnUKN+AbTkuXdxiL4s2cy/25IKAD4lneWSXKD/zb4abScakpl6huTNotF' + #13#10 +
'4qtiAyb3hvA23ahifY42/N4v/Q6AczPeByEK9aIlgTjEWHBruCQ6sp5HRYWUTc0A' + #13#10 +
'b4urJq6ZiWqK4e4BupyzkFFSsS4HDIubjY1CJQuzgVkfNDdMZS80D+lN65NB5bV3' + #13#10 +
'KY5Kmew2MpV8/TXliYCl8EWo2ipNPqd0n82AR51itpwN5LKFlI/YdIkBMNnhZNpB' + #13#10 +
'2lZZbVV6X0I6JHhUFQpz4e+/87XEL2JWmzrtZQN2aHW3NuKcggjMOV+MNQogvfst' + #13#10 +
'9Okn5XCYsKfPpLXEHbUbs+KQWhYs3UMgfhcYAl3DAeVCYsbysYV/74ZLkho='     + #13#10 +
'-----END CERTIFICATE-----';

DEBUG_CERTIFICATE_FINGERPRINT = '92:3F:50:8D:CC:CB:13:AF:47:70:62:60:11:BB:0C:23:D0:E3:DD:C9:32:C5:BD:47:3E:' +
                                '85:F0:89:7C:C1:A4:9D:C4:03:94:28:8E:50:41:69:55:19:B7:D0:7A:71:EE:FC:DA:C5:' +
                                '54:2F:A2:E5:0D:86:3E:7E:87:10:5A:16:09:AD';

DEBUG_PEER_CERTIFICATE_FINGERPRINT = 'D7:7F:D9:8A:C1:CB:44:8F:D7:F1:8D:98:BA:BD:BE:06:81:F3:4F:B8:93:41:E9:6B:AF:' +
                                     'D4:D6:C9:3B:C1:63:F3:E7:52:1C:00:04:AD:F8:2D:7D:38:18:70:75:09:76:E7:2E:D7:' +
                                     'FB:AD:D3:87:4C:72:69:4C:D2:18:C2:C2:1C:5A';

implementation

end.
