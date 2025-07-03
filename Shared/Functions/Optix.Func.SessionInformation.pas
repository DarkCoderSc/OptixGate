{******************************************************************************}
{                                                                              }
{            __________.__                                                     }
{            \______   \  |_________  ____________ ____   ____                 }
{             |     ___/  |  \_  __ \/  _ \___   // __ \ /    \                }
{             |    |   |   Y  \  | \(  <_> )    /\  ___/|   |  \               }
{             |____|   |___|  /__|   \____/_____ \\___  >___|  /               }
{             \/                  \/    \/     \/                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://www.phrozen.io/                                    }
{                   https://github.com/PhrozenIO                               }
{                   License: Private                                           }
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
    FComputer       : String;
    FProcessId      : Cardinal;
    FImagePath      : String;
    FArchitecture   : TProcessArchitecture;
    FElevatedStatus : TElevatedStatus;

    {@M}
    function GetProcessDetail() : String;
    function GetElevatedStatusString() : String;
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

    property ProcessDetail      : String read GetProcessDetail;
    property ElevatedStatus_STR : String read GetElevatedStatusString;
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
  FComputer       := TOptixInformationGathering.TryGetComputerName();
  FElevatedStatus := TProcessInformation.IsElevated();
end;

{ TOptixSessionInformation.Serialize }
function TOptixSessionInformation.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['Username']       := FUsername;
  result.S['Computer']       := FComputer;
  result.S['WindowsVersion'] := FWindowsVersion;
  result.I['ProcessId']      := FProcessId;
  result.I['Architecture']   := Integer(FArchitecture);
  result.S['ImagePath']      := FImagePath;
  result.I['ElevatedStatus'] := Integer(FElevatedStatus);
end;

{ TOptixSessionInformation.DeSerialize }
procedure TOptixSessionInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  if not Assigned(ASerializedObject) then
    Exit();

  FUsername       := ASerializedObject.S['Username'];
  FComputer       := ASerializedObject.S['Computer'];
  FWindowsVersion := ASerializedObject.S['WindowsVersion'];
  FProcessId      := ASerializedObject.I['ProcessId'];
  FArchitecture   := TProcessArchitecture(ASerializedObject.I['Architecture']);
  FImagePath      := ASerializedObject.S['ImagePath'];
  FElevatedStatus := TElevatedStatus(ASerializedObject.I['ElevatedStatus']);
end;

{ TOptixSessionInformation.DeSerialize }
procedure TOptixSessionInformation.Assign(ASource : TPersistent);
begin
  if ASource is TOptixSessionInformation then begin
    FUsername        := TOptixSessionInformation(ASource).FUsername;
    FComputer        := TOptixSessionInformation(ASource).FComputer;
    FWindowsVersion  := TOptixSessionInformation(ASource).FWindowsVersion;
    FProcessId       := TOptixSessionInformation(ASource).FProcessId;
    FArchitecture    := TOptixSessionInformation(ASource).FArchitecture;
    FImagePath       := TOptixSessionInformation(ASource).FImagePath;
    FElevatedStatus  := TOptixSessionInformation(ASource).FElevatedStatus;
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

end.
