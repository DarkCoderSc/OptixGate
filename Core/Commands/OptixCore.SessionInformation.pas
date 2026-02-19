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



unit OptixCore.SessionInformation;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Winapi.Windows,

  OptixCore.System.InformationGathering, OptixCore.System.Process, OptixCore.WinApiEx, OptixCore.Commands.Base,
  OptixCore.Classes;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixCommandReceiveSessionInformation = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FWindowsVersion : String;

    [OptixSerializableAttribute]
    FUsername : String;

    [OptixSerializableAttribute]
    FUserSid : String;

    [OptixSerializableAttribute]
    FComputer : String;

    [OptixSerializableAttribute]
    FProcessId : Cardinal;

    [OptixSerializableAttribute]
    FImagePath : String;

    [OptixSerializableAttribute]
    FArchitecture : TProcessorArchitecture;

    [OptixSerializableAttribute]
    FWindowsArchitecture : TProcessorArchitecture;

    [OptixSerializableAttribute]
    FElevatedStatus : TElevatedStatus;

    [OptixSerializableAttribute]
    FLangroup : String;

    [OptixSerializableAttribute]
    FDomainName : String;

    [OptixSerializableAttribute]
    FIsInAdminGroup : Boolean;

    {@M}
    function GetProcessDetail() : String;
    function GetElevatedStatusString() : String;
    function CheckIfSystemUser() : Boolean;
  public
    {@M}
    procedure Assign(ASource : TPersistent); override;
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}

    {@G}
    property Architecture        : TProcessorArchitecture read FArchitecture;
    property WindowsArchitecture : TProcessorArchitecture read FWindowsArchitecture;
    property WindowsVersion      : String                 read FWindowsVersion;
    property ProcessId           : Cardinal               read FProcessId;
    property Username            : String                 read FUsername;
    property Computer            : String                 read FComputer;
    property ImagePath           : String                 read FImagePath;
    property ElevatedStatus      : TElevatedStatus        read FElevatedStatus;
    property Langroup            : String                 read FLangroup;
    property DomainName          : String                 read FDomainName;
    property IsInAdminGroup      : Boolean                read FIsInAdminGroup;
    property UserSid             : String                 read FUserSid;

    property ProcessDetail      : String  read GetProcessDetail;
    property ElevatedStatus_STR : String  read GetElevatedStatusString;
    property IsSystem           : Boolean read CheckIfSystemUser;
  end;

implementation

{$IFNDEF SERVER}
procedure TOptixCommandReceiveSessionInformation.DoAction();
begin
  FWindowsVersion      := TOSVersion.ToString();
  FArchitecture        := TOptixInformationGathering.CurrentProcessArchitecture;
  FProcessId           := GetCurrentProcessId();
  FImagePath           := GetModuleName(hInstance);
  FUserName            := TOptixInformationGathering.TryGetUserName();
  FUserSid             := TOptixInformationGathering.TryGetCurrentUserSid();
  FComputer            := TOptixInformationGathering.TryGetComputerName();
  FElevatedStatus      := TProcessHelper.IsElevated();
  FLangroup            := TOptixInformationGathering.GetLangroup;
  FDomainName          := TOptixInformationGathering.GetDomainName;
  FIsInAdminGroup      := TOptixInformationGathering.TryIsCurrentUserInAdminGroup();
  FWindowsArchitecture := TOptixInformationGathering.GetWindowsArchitecture();
end;
{$ENDIF}

procedure TOptixCommandReceiveSessionInformation.Assign(ASource : TPersistent);
begin
  if ASource is TOptixCommandReceiveSessionInformation then begin
    FUsername            := TOptixCommandReceiveSessionInformation(ASource).FUsername;
    FUserSid             := TOptixCommandReceiveSessionInformation(ASource).FUserSid;
    FComputer            := TOptixCommandReceiveSessionInformation(ASource).FComputer;
    FWindowsVersion      := TOptixCommandReceiveSessionInformation(ASource).FWindowsVersion;
    FProcessId           := TOptixCommandReceiveSessionInformation(ASource).FProcessId;
    FArchitecture        := TOptixCommandReceiveSessionInformation(ASource).FArchitecture;
    FImagePath           := TOptixCommandReceiveSessionInformation(ASource).FImagePath;
    FElevatedStatus      := TOptixCommandReceiveSessionInformation(ASource).FElevatedStatus;
    FLangroup            := TOptixCommandReceiveSessionInformation(ASource).Langroup;
    FDomainName          := TOptixCommandReceiveSessionInformation(ASource).DomainName;
    FIsInAdminGroup      := TOptixCommandReceiveSessionInformation(ASource).IsInAdminGroup;
    FWindowsArchitecture := TOptixCommandReceiveSessionInformation(ASource).WindowsArchitecture;
  end else
    inherited;
end;

function TOptixCommandReceiveSessionInformation.GetProcessDetail() : String;
begin
  result := Format('%d - %s (%s)', [
    FProcessId,
    ExtractFileName(FImagePath),
    ProcessArchitectureToString(FArchitecture)
  ]);
end;

function TOptixCommandReceiveSessionInformation.GetElevatedStatusString() : String;
begin
  result := ElevatedStatusToString(FElevatedStatus);
end;

function TOptixCommandReceiveSessionInformation.CheckIfSystemUser() : Boolean;
begin
  result := String.Compare(FUserSid, 'S-1-5-18', True) =  0;
end;

end.
