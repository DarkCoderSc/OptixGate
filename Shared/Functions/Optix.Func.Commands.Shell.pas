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


unit Optix.Func.Commands.Shell;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  XSuperObject,

  Optix.Func.Commands.Base, Optix.Shared.Classes;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixCommandShell = class(TOptixCommand);

  TOptixStartShellInstance = class(TOptixCommandShell);

  TOptixShellInstance = class(TOptixCommandShell)
  private
    [OptixSerializableAttribute]
    FInstanceId : TGUID;
  public
    {@C}
    constructor Create(const AInstanceId : TGUID);

    {@G}
    property InstanceId : TGUID read FInstanceId;
  end;

  TOptixTerminateShellInstance = class(TOptixShellInstance);
  TOptixBreakShellInstance = class(TOptixShellInstance);

  TOptixStdinShellInstance = class(TOptixShellInstance)
  private
    [OptixSerializableAttribute]
    FCommandLine : String;
  public

    {@C}
    constructor Create(const AInstanceId : TGUID; const ACommandLine : String); overload;

    {@G}
    property CommandLine : String read FCommandLine;
  end;

  TOptixShellOutput = class(TOptixCommand)
  private
    [OptixSerializableAttribute]
    FInstanceId : TGUID;

    [OptixSerializableAttribute]
    FOutput : String;
  public
    {@C}
    constructor Create(const AGroupId : TGUID; const AOutput : String; const AInstanceId : TGUID);

    {@G}
    property InstanceId : TGUID  read FInstanceId;
    property Output     : String read FOutput;
  end;

implementation

(***********************************************************************************************************************

  TOptixShellInstance

***********************************************************************************************************************)

{ TOptixShellInstance.Create }
constructor TOptixShellInstance.Create(const AInstanceId : TGUID);
begin
  inherited Create();
  ///

  FInstanceId := AInstanceId;
end;

(***********************************************************************************************************************

  TOptixStdinShellInstance

***********************************************************************************************************************)

{ TOptixStdinShellInstance.Create }
constructor TOptixStdinShellInstance.Create(const AInstanceId : TGUID; const ACommandLine : String);
begin
  inherited Create(AInstanceId);
  ///

  FCommandLine := ACommandLine;
end;

(***********************************************************************************************************************

  TOptixShellOutput

***********************************************************************************************************************)

{ TOptixShellOutput.Create }
constructor TOptixShellOutput.Create(const AGroupId : TGUID; const AOutput : String; const AInstanceId : TGUID);
begin
  inherited Create();
  ///

  FOutput     := AOutput;
  FInstanceId := AInstanceId;
  FWindowGUID := AGroupId;
end;

end.
