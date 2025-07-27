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

unit Optix.Constants;

interface

uses VCL.Graphics, Winapi.Windows;

const
 (* IMAGES *)
 IMAGE_USER                = 0;
 IMAGE_USER_ADMIN          = 1;
 IMAGE_USER_ELEVATED       = 2;
 IMAGE_USER_SYSTEM         = 3;
 IMAGE_PROCESS_SELF        = 6;
 IMAGE_PROCESS             = 7;
 IMAGE_PROCESS_X86_32      = 8;
 IMAGE_PROCESS_X86_64      = 9;
 IMAGE_EXCEPTION           = 10;
 IMAGE_FORM_CONTROL        = 11;
 IMAGE_FORM_CONTROL_ACTIVE = 12;
 IMAGE_FORM_CONTROL_DATA   = 13;
 IMAGE_FORM_CONTROL_CLOSED = 14;
 IMAGE_DRIVE               = 15;
 IMAGE_DRIVE_USB           = 16;
 IMAGE_DRIVE_HARDWARE      = 17;
 IMAGE_DRIVE_NO_ROOT       = 18;
 IMAGE_DRIVE_NETWORK       = 19;
 IMAGE_DRIVE_UNKNOWN       = 20;
 IMAGE_DRIVE_CD            = 21;
 IMAGE_FILE_DOWNLOAD       = 22;
 IMAGE_FILE_UPLOAD         = 23;
 IMAGE_FILE_QUEUE          = 24;
 IMAGE_FILE_TRANSFERING    = 25;
 IMAGE_FLAG_GREEN          = 26;
 IMAGE_FILE_TRANSFERED     = IMAGE_FLAG_GREEN;
 IMAGE_FILE_TRANSFER_ERROR = 27;

var
 (* COLORS *)
 COLOR_LIST_BLUE     : TColor;
 COLOR_LIST_PURPLE   : TColor;
 COLOR_LIST_LIMY     : TColor;
 COLOR_LIST_RED      : TColor;
 COLOR_LIST_GRAY     : TColor;

 COLOR_USER_ELEVATED : TColor;
 COLOR_USER_SYSTEM   : TColor;

implementation

initialization
  COLOR_LIST_BLUE     := RGB(234, 249, 254);
  COLOR_USER_ELEVATED := COLOR_LIST_BLUE;

  COLOR_LIST_PURPLE   := RGB(242, 228, 247);
  COLOR_USER_SYSTEM   := COLOR_LIST_PURPLE;

  COLOR_LIST_LIMY     := RGB(220, 254, 215);
  COLOR_LIST_RED      := RGB(254, 220, 225);

  COLOR_LIST_GRAY     := RGB(250, 250, 250);

end.
