{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                              Project: Optix Neo                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{    I dedicate this work to my daughter & wife                                }
{                                                                              }
{******************************************************************************}

// TODO: Use variants instead of overloading Read / Write methods.

unit NeoFlat.SettingHandler;

interface

uses System.Classes, XSuperObject, Generics.Collections;

type
  TSettingHandlerEvent = (
    sheLoaded,
    sheSaved
  );

  TOnSettingHandlerNotify = procedure(Sender : TObject; const AEvent : TSettingHandlerEvent) of object;

  TFlatSettingHandler = class(TComponent)
  private
    FOutputPath  : String;
    FJsonSetting : ISuperObject;
    FNotifiers   : TList<TOnSettingHandlerNotify>;

    {@M}
    procedure SetupJson();
    function GetOrCreateMainGroup(const AGroupName : String) : ISuperObject;
    procedure BroadcastEvent(const AEvent : TSettingHandlerEvent);
  public
    {@M}
    procedure Save();
    procedure Load();
    procedure Loaded(); override;

    procedure UpdateOrCreateNode(const ANodeName : String; ANode : ISuperObject);
    procedure UpdateOrCreateArray(const AArrayName : String; AArray : ISuperArray);

    function GetNode(const ANodeName : String) : ISuperObject;
    function GetArray(const AArrayName : String) : ISuperArray;

    procedure Write(const AName, AValue : String; const AGroupName : String = ''); overload;
    procedure Write(const AName : String; AValue : Int64; const AGroupName : String = ''); overload;
    procedure Write(const AName : String; AValue : Boolean; const AGroupName : String = ''); overload;

    function Read(const AName, ADefault : String; const AGroupName : String = '') : String; overload;
    function Read(const AName : String; ADefault : Int64; const AGroupName : String = '') : Int64; overload;
    function Read(const AName : String; ADefault : Boolean; const AGroupName : String = '') : Boolean; overload;

    procedure RegisterCallback(const ACallback : TOnSettingHandlerNotify);
    procedure UnregisterCallback(const ACallback : TOnSettingHandlerNotify);

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property OutputPath : String read FOutputPath write FOutputPath;
  end;

implementation

uses System.SysUtils, Winapi.Windows;

{ TFlatSettingHandler.BroadcastEvent }

procedure TFlatSettingHandler.BroadcastEvent(const AEvent : TSettingHandlerEvent);
var ACallback : TOnSettingHandlerNotify;
begin
  for ACallback in FNotifiers do
    ACallback(self, AEvent);
end;

{ TFlatSettingHandler.RegisterCallback }

procedure TFlatSettingHandler.RegisterCallback(const ACallback : TOnSettingHandlerNotify);
begin
  if Assigned(ACallback) then
    FNotifiers.Add(ACallback);
end;

{ TFlatSettingHandler.UnregisterCallback }

procedure TFlatSettingHandler.UnregisterCallback(const ACallback : TOnSettingHandlerNotify);
begin
  if Assigned(ACallback) then
    FNotifiers.Remove(ACallback);
end;

{ TFlatSettingHandler.SetupJson }

procedure TFlatSettingHandler.SetupJson();
begin
  if not Assigned(FJsonSetting) then
    FJsonSetting := TSuperObject.Create();

  if not FJsonSetting.Contains('main') then
    FJsonSetting.O['main'] := SO();
end;

{ TFlatSettingHandler.GetOrCreateMainGroup }

function TFlatSettingHandler.GetOrCreateMainGroup(const AGroupName : String) : ISuperObject;
var AMain : ISuperObject;
begin
  self.SetupJson();
  ///

  AMain := FJsonSetting.O['main'];

  if (AGroupName <> '') then begin
    if AMain.Contains(AGroupName) then
      result := AMain.O[AGroupName]
    else begin
      result := TSuperObject.Create();

      AMain.O[AGroupName] := result;
    end;
  end else
    result := AMain;
end;

{ TFlatSettingHandler.Write }

procedure TFlatSettingHandler.Write(const AName, AValue : String; const AGroupName : String = '');
var AGroup : ISuperObject;
begin
  AGroup := self.GetOrCreateMainGroup(AGroupName);

  AGroup.S[AName] := AValue;
end;

{ TFlatSettingHandler.Write }

procedure TFlatSettingHandler.Write(const AName : String; AValue : Int64; const AGroupName : String = '');
var AGroup : ISuperObject;
begin
  AGroup := self.GetOrCreateMainGroup(AGroupName);

  AGroup.I[AName] := AValue;
end;

{ TFlatSettingHandler.Write }

procedure TFlatSettingHandler.Write(const AName : String; AValue : Boolean; const AGroupName : String = '');
var AGroup : ISuperObject;
begin
  AGroup := self.GetOrCreateMainGroup(AGroupName);

  AGroup.B[AName] := AValue;
end;

{ TFlatSettingHandler.Read }

function TFlatSettingHandler.Read(const AName, ADefault : String; const AGroupName : String = '') : String;
var AGroup : ISuperObject;
begin
  result := ADefault;
  ///

  AGroup := self.GetOrCreateMainGroup(AGroupName);

  if AGroup.Contains(AName) then
    try
      result := AGroup.S[AName];
    except

    end;
end;

{ TFlatSettingHandler.Read }

function TFlatSettingHandler.Read(const AName : String; ADefault : Int64; const AGroupName : String = '') : Int64;
var AGroup : ISuperObject;
begin
  result := ADefault;
  ///

  AGroup := self.GetOrCreateMainGroup(AGroupName);

  if AGroup.Contains(AName) then
    try
      result := AGroup.I[AName];
    except

    end;
end;

{ TFlatSettingHandler.Read }

function TFlatSettingHandler.Read(const AName : String; ADefault : Boolean; const AGroupName : String = '') : Boolean;
var AGroup : ISuperObject;
begin
  result := ADefault;
  ///

  AGroup := self.GetOrCreateMainGroup(AGroupName);

  if AGroup.Contains(AName) then
    try
      result := AGroup.B[AName];
    except

    end;
end;


{ TFlatSettingHandler.Loaded }

procedure TFlatSettingHandler.Loaded();
begin
  inherited Loaded();
  ///

  if (csDesigning in ComponentState) then
    Exit;

  if FOutputPath <> '' then
    self.Load();
end;

{ TFlatSettingHandler.Save }

procedure TFlatSettingHandler.Load();
var AFileStream : TFileStream;
    ABytes      : TBytes;
begin
  self.SetupJson();
  ///

  if not FileExists(FOutputPath) then
    Exit();

  AFileStream := TFileStream.Create(FOutputPath, fmOpenRead);
  try
    if AFileStream.Size = 0 then
      Exit();
    ///

    AFileStream.Position := 0;

    SetLength(ABytes, AFileStream.Size);
    try
      AFileStream.Read(ABytes[0], AFileStream.Size);
      try
        FJsonSetting := TSuperObject.Create(TEncoding.Unicode.GetString(ABytes));
      except

      end;
    finally
      SetLength(ABytes, 0);
    end;
  finally
    if Assigned(AFileStream) then
      FreeAndNil(AFileStream);
  end;

  ///
  self.BroadcastEvent(sheLoaded);
end;

{ TFlatSettingHandler.Save }

procedure TFlatSettingHandler.Save();
var AFileStream : TFileStream;
    AString     : String;
begin
  self.SetupJson();
  ///

  AFileStream := TFileStream.Create(
    FOutputPath,
    fmCreate or
    fmShareDenyWrite
  );
  try
    AString := FJsonSetting.AsJson(True);

    AFileStream.Write(AString[1], Length(AString) * SizeOf(WideChar));
  finally
    if Assigned(AFileStream) then
      FreeAndNil(AFileStream);
  end;

  ///
  self.BroadcastEvent(sheSaved);
end;

{ TFlatSettingHandler.Create }

constructor TFlatSettingHandler.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FOutputPath  := '';
  FJsonSetting := TSuperObject.Create();

  FNotifiers   := TList<TOnSettingHandlerNotify>.Create();
end;

destructor TFlatSettingHandler.Destroy();
begin
  if not (csDesigning in ComponentState) then
    self.Save();

  if Assigned(FNotifiers) then
    FreeAndNil(FNotifiers);

  ///
  inherited Destroy();
end;

{ TFlatSettingHandler.UpdateOrCreateNode }

procedure TFlatSettingHandler.UpdateOrCreateNode(const ANodeName : String; ANode : ISuperObject);
begin
  if not Assigned(ANode) then
    ANode := TSuperObject.Create();

  self.SetupJson();

  FJsonSetting.O[ANodeName] := ANode;
end;

{ TFlatSettingHandler.UpdateOrCreateArray }

procedure TFlatSettingHandler.UpdateOrCreateArray(const AArrayName : String; AArray : ISuperArray);
begin
  if not Assigned(AArray) then
    AArray := TSuperArray.Create();

  self.SetupJson();

  FJsonSetting.A[AArrayName] := AArray;
end;

{ TFlatSettingHandler.GetNode }

function TFlatSettingHandler.GetNode(const ANodeName : String) : ISuperObject;
begin
  result := nil;

  self.SetupJson();

  if FJsonSetting.Contains(ANodeName) then
    result := FJsonSetting.O[ANodeName];
end;

{ TFlatSettingHandler.GetArray }

function TFlatSettingHandler.GetArray(const AArrayName : String) : ISuperArray;
begin
  result := nil;

  self.SetupJson();

  if FJsonSetting.Contains(AArrayName) then
    result := FJsonSetting.A[AArrayName];
end;

end.
