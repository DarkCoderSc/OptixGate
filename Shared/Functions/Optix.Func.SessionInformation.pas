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
{                   License: Apache License 2.0                                }
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

unit Optix.Func.SessionInformation;

interface

uses Winapi.Windows, System.Classes, System.SysUtils, Optix.Interfaces,
     XSuperObject, Optix.Func.Response, Optix.InformationGathering.Helper,
     Optix.InformationGathering.Process;

type
  TOptixSessionInformation = class(TOptixResponse)
  private
    FWindowsVersion : String;
    FUsername       : String;
    FUserSid        : String;
    FComputer       : String;
    FProcessId      : Cardinal;
    FImagePath      : String;
    FArchitecture   : TProcessArchitecture;
    FElevatedStatus : TElevatedStatus;
    FLangroup       : String;
    FDomainName     : String;
    FIsInAdminGroup : Boolean;

    {@M}
    function GetProcessDetail() : String;
    function GetElevatedStatusString() : String;
    function CheckIfSystemUser() : Boolean;
  protected
    {@M}
    procedure Refresh(); override;
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Architecture   : TProcessArchitecture read FArchitecture;
    property WindowsVersion : String               read FWindowsVersion;
    property ProcessId      : Cardinal             read FProcessId;
    property Username       : String               read FUsername;
    property Computer       : String               read FComputer;
    property ImagePath      : String               read FImagePath;
    property ElevatedStatus : TElevatedStatus      read FElevatedStatus;
    property Langroup       : String               read FLangroup;
    property DomainName     : String               read FDomainName;
    property IsInAdminGroup : Boolean              read FIsInAdminGroup;
    property UserSid        : String               read FUserSid;

    property ProcessDetail      : String  read GetProcessDetail;
    property ElevatedStatus_STR : String  read GetElevatedStatusString;
    property IsSystem           : Boolean read CheckIfSystemUser;
  end;

implementation

{ TOptixSessionInformation.Refresh }
procedure TOptixSessionInformation.Refresh();
begin
  FWindowsVersion := TOSVersion.ToString();
  FArchitecture   := TOptixInformationGathering.CurrentProcessArchitecture;
  FProcessId      := GetCurrentProcessId();
  FImagePath      := GetModuleName(hInstance);
  FUserName       := TOptixInformationGathering.TryGetUserName();
  FUserSid        := TOptixInformationGathering.TryGetCurrentUserSid();
  FComputer       := TOptixInformationGathering.TryGetComputerName();
  FElevatedStatus := TProcessInformationHelper.IsElevated();
  FLangroup       := TOptixInformationGathering.GetLangroup;
  FDomainName     := TOptixInformationGathering.GetDomainName;
  FIsInAdminGroup := TOptixInformationGathering.TryIsCurrentUserInAdminGroup();
end;

{ TOptixSessionInformation.Serialize }
function TOptixSessionInformation.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['Username']       := FUsername;
  result.S['UserSid']        := FUserSid;
  result.S['Computer']       := FComputer;
  result.S['WindowsVersion'] := FWindowsVersion;
  result.I['ProcessId']      := FProcessId;
  result.I['Architecture']   := Integer(FArchitecture);
  result.S['ImagePath']      := FImagePath;
  result.I['ElevatedStatus'] := Integer(FElevatedStatus);
  result.S['Langroup']       := FLangroup;
  result.S['DomainName']     := FDomainName;
  result.B['IsInAdminGroup'] := FIsInAdminGroup;
end;

{ TOptixSessionInformation.DeSerialize }
procedure TOptixSessionInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  if not Assigned(ASerializedObject) then
    Exit();

  FUsername       := ASerializedObject.S['Username'];
  FUserSid        := ASerializedObject.S['UserSid'];
  FComputer       := ASerializedObject.S['Computer'];
  FWindowsVersion := ASerializedObject.S['WindowsVersion'];
  FProcessId      := ASerializedObject.I['ProcessId'];
  FArchitecture   := TProcessArchitecture(ASerializedObject.I['Architecture']);
  FImagePath      := ASerializedObject.S['ImagePath'];
  FElevatedStatus := TElevatedStatus(ASerializedObject.I['ElevatedStatus']);
  FLangroup       := ASerializedObject.S['Langroup'];
  FDomainName     := ASerializedObject.S['DomainName'];
  FIsInAdminGroup := ASerializedObject.B['IsInAdminGroup'];
end;

{ TOptixSessionInformation.DeSerialize }
procedure TOptixSessionInformation.Assign(ASource : TPersistent);
begin
  if ASource is TOptixSessionInformation then begin
    FUsername        := TOptixSessionInformation(ASource).FUsername;
    FUserSid         := TOptixSessionInformation(ASource).FUserSid;
    FComputer        := TOptixSessionInformation(ASource).FComputer;
    FWindowsVersion  := TOptixSessionInformation(ASource).FWindowsVersion;
    FProcessId       := TOptixSessionInformation(ASource).FProcessId;
    FArchitecture    := TOptixSessionInformation(ASource).FArchitecture;
    FImagePath       := TOptixSessionInformation(ASource).FImagePath;
    FElevatedStatus  := TOptixSessionInformation(ASource).FElevatedStatus;
    FLangroup        := TOptixSessionInformation(ASource).Langroup;
    FDomainName      := TOptixSessionInformation(ASource).DomainName;
    FIsInAdminGroup  := TOptixSessionInformation(ASource).IsInAdminGroup;
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
