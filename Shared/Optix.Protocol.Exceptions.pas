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


unit Optix.Protocol.Exceptions;

interface

uses System.SysUtils;

type
  TPreflightErrorCode = (
    pecSuccess,
    pecVersionMismatch
    {$IFDEF USETLS},
    pecUntrustedPeer
    {$ENDIF}
  );

  EOptixPreflightException = class(Exception)
  private
    FErrorCode : TPreflightErrorCode;
  public
    {@C}
    constructor Create(const AMessage : String; const AErrorCode : TPreflightErrorCode);

    {@G}
    property ErrorCode : TPreflightErrorCode read FErrorCode;
  end;

  function PreflightErrorCodeToString(const AValue : TPreflightErrorCode) : String;

implementation

(* Local *)

{ _.PreflightErrorCodeToString }
function PreflightErrorCodeToString(const AValue : TPreflightErrorCode) : String;
begin
  case AValue of
    pecVersionMismatch : result := 'Protocol Version Mismatch';
    {$IFDEF USETLS}
    pecUntrustedPeer   : result := 'Untrusted Peer Certificate';
    {$ENDIF}
    else
      result := 'Success';
  end;
end;

(* EOptixPreflightException *)

{ EOptixPreflightException.Create }
constructor EOptixPreflightException.Create(const AMessage : String; const AErrorCode : TPreflightErrorCode);
begin
  FErrorCode := AErrorCode;

  ///
  inherited Create(AMessage);
end;

end.
