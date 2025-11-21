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


unit Optix.Func.SessionInformation;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Winapi.Windows,

  XSuperObject,

  Optix.InformationGathering.Helper, Optix.Process.Helper, Optix.WinApiEx, Optix.Func.Commands.Base,
  Optix.Shared.Classes;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixSessionInformation = class(TOptixCommandActionResponse)
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
    {@C}
    procedure DoAction(); override;

    {@M}
    procedure Assign(ASource : TPersistent); override;

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

{ TOptixSessionInformation.DoAction }
procedure TOptixSessionInformation.DoAction();
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

{ TOptixSessionInformation.DeSerialize }
procedure TOptixSessionInformation.Assign(ASource : TPersistent);
begin
  if ASource is TOptixSessionInformation then begin
    FUsername            := TOptixSessionInformation(ASource).FUsername;
    FUserSid             := TOptixSessionInformation(ASource).FUserSid;
    FComputer            := TOptixSessionInformation(ASource).FComputer;
    FWindowsVersion      := TOptixSessionInformation(ASource).FWindowsVersion;
    FProcessId           := TOptixSessionInformation(ASource).FProcessId;
    FArchitecture        := TOptixSessionInformation(ASource).FArchitecture;
    FImagePath           := TOptixSessionInformation(ASource).FImagePath;
    FElevatedStatus      := TOptixSessionInformation(ASource).FElevatedStatus;
    FLangroup            := TOptixSessionInformation(ASource).Langroup;
    FDomainName          := TOptixSessionInformation(ASource).DomainName;
    FIsInAdminGroup      := TOptixSessionInformation(ASource).IsInAdminGroup;
    FWindowsArchitecture := TOptixSessionInformation(ASource).WindowsArchitecture;
  end else
    inherited;
end;

{ TOptixSessionInformation.GetProcessDetail }
function TOptixSessionInformation.GetProcessDetail() : String;
begin
  result := Format('%d - %s (%s)', [
    FProcessId,
    ExtractFileName(FImagePath),
    ProcessArchitectureToString(FArchitecture)
  ]);
end;

{ TOptixSessionInformation.GetElevatedStatusString }
function TOptixSessionInformation.GetElevatedStatusString() : String;
begin
  result := ElevatedStatusToString(FElevatedStatus);
end;

{ TOptixSessionInformation.CheckIfSystemUser() }
function TOptixSessionInformation.CheckIfSystemUser() : Boolean;
begin
  result := String.Compare(FUserSid, 'S-1-5-18', True) =  0;
end;

end.
