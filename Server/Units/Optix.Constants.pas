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
{  Authorship (No AI):                                                         }
{  -------------------                                                         }
{   All code contained in this unit was written and developed by the author    }
{   without the assistance of artificial intelligence systems, large language  }
{   models (LLMs), or automated code generation tools. Any external libraries  }
{   or frameworks used comply with their respective licenses.	                 }
{                                                                              }
{******************************************************************************}



unit Optix.Constants;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  Winapi.Windows,

  VCL.Graphics;
// ---------------------------------------------------------------------------------------------------------------------

const
 (* IMAGES *)
 IMAGE_USER                         = 0;
 IMAGE_APP                          = 1;
 IMAGE_EMO_TONG                     = 2;
 IMAGE_ANCHOR                       = 3;
 IMAGE_CONTROL_APP                  = 4;
 IMAGE_TERMINAL                     = 6;
 IMAGE_RIGHT_ARROW                  = 7;
 IMAGE_BULLET_GREEN                 = 8;
 IMAGE_BULLET_YELLOW                = 9;
 IMAGE_BULLET_ORANGE                = 10;
 IMAGE_BULLET_BLUE                  = 11;
 IMAGE_BULLET_PINK                  = 12;
 IMAGE_BULLET_PURPLE                = 13;
 IMAGE_BULLET_RED                   = 14;
 IMAGE_BULLET_WHITE                 = 15;
 IMAGE_BULLET_BLACK                 = 16;
 IMAGE_APP_ERROR                    = 24;
 IMAGE_FOLDER_NORMAL                = 25;
 IMAGE_FOLDER_PREV                  = IMAGE_FOLDER_NORMAL;
 IMAGE_FOLDER_FULLACCESS            = 26;
 IMAGE_FOLDER_READONLY              = 27;
 IMAGE_FOLDER_WRITEONLY             = 28;
 IMAGE_FOLDER_EXECONLY              = 29;
 IMAGE_FOLDER_DENIED                = 30;
 IMAGE_PAGE                         = 31;
 IMAGE_PAGE_SYS                     = 32;
 IMAGE_TASK                         = 33;
 IMAGE_BLUE_ARROW_LEFT              = 34;
 IMAGE_BLUE_ARROW_RIGHT             = 35;
 IMAGE_TRANSMIT                     = 36;
 IMAGE_SERVER                       = 37;
 IMAGE_COFEE                        = 38;
 IMAGE_FOLDER_WRENCH                = 39;
 IMAGE_BRICK                        = 40;
 IMAGE_BRICK_WARNING                = 41;
 IMAGE_BRICK_ERROR                  = 42;
 IMAGE_CERTIFICATE                  = 43;
 IMAGE_BUG                          = 44;
 IMAGE_SERVER_RUNNING               = 45;
 IMAGE_SERVER_ERROR                 = 46;
 IMAGE_SERVER_STOPPED               = 47;
 IMAGE_LOCK                         = 48;
 IMAGE_COMPUTER                     = 49;
 IMAGE_COMPUTER_LINKED              = 50;
 IMAGE_COMPUTER_ERROR               = 51;

var
 (* COLORS *)
 COLOR_LIST_BLUE         : TColor;
 COLOR_LIST_DARKER_BLUE  : TColor;
 COLOR_LIST_PURPLE       : TColor;
 COLOR_LIST_GREEN        : TColor;
 COLOR_LIST_YELLOW       : TColor;
 COLOR_LIST_RED          : TColor;
 COLOR_LIST_GRAY         : TColor;
 COLOR_LIST_ORANGE       : TColor;

 COLOR_TEXT_WARNING      : TColor;

 COLOR_USER_ELEVATED     : TColor;
 COLOR_USER_SYSTEM       : TColor;

 COLOR_FILE_WRITE_ONLY   : TColor;
 COLOR_FILE_READ_ONLY    : TColor;
 COLOR_FILE_EXECUTE_ONLY : TColor;
 COLOR_FILE_ALL_ACCESS   : TColor;
 COLOR_FILE_NO_ACCESS    : TColor;

implementation

initialization
  COLOR_LIST_GREEN        := RGB(227, 238, 197);
  COLOR_LIST_YELLOW       := RGB(255, 254, 224);
  COLOR_LIST_BLUE         := RGB(225, 242, 255);
  COLOR_LIST_DARKER_BLUE  := RGB(190, 220, 245);
  COLOR_LIST_PURPLE       := RGB(241, 230, 254);
  COLOR_LIST_RED          := RGB(255, 230, 230);
  COLOR_LIST_GRAY         := RGB(244, 244, 244);
  COLOR_LIST_ORANGE       := RGB(255, 240, 225);

  COLOR_USER_ELEVATED     := COLOR_LIST_BLUE;
  COLOR_USER_SYSTEM       := COLOR_LIST_PURPLE;

  COLOR_TEXT_WARNING      := RGB(255, 140, 0);

  COLOR_FILE_WRITE_ONLY   := COLOR_LIST_PURPLE;
  COLOR_FILE_READ_ONLY    := COLOR_LIST_BLUE;
  COLOR_FILE_EXECUTE_ONLY := COLOR_LIST_ORANGE;
  COLOR_FILE_ALL_ACCESS   := COLOR_LIST_GREEN;
  COLOR_FILE_NO_ACCESS    := COLOR_LIST_RED;


end.
