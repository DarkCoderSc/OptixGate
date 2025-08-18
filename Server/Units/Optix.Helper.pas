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
{                   https://github.com/darkcodersc                             }
{                   License: GPL v3                                            }
{                                                                              }
{                                                                              }
{    I dedicate this work to my daughter & wife                                }
{                                                                              }
{******************************************************************************}

unit Optix.Helper;

interface

uses System.Classes, System.SysUtils, System.DateUtils, System.Math, System.TimeSpan, Winapi.ShellAPI, VCL.Controls,
     System.RegularExpressions;

// Format Utilities
function FormatInt(const AInteger : Integer) : String;

// Date Utilities
function ElapsedTime(const ADays, AHours, AMinutes, ASeconds : UInt64) : String; overload;
function ElapsedTime(const AMilliseconds : UInt64) : String; overload;
function ElapsedDateTime(const AFirstDateTime, ASecondDateTime : TDateTime) : String;

function DefaultIfEmpty(const AValue : String; const ADefault : String = '-') : String;

function ReadResourceString(const AResourceName : String) : String;
function TryReadResourceString(const AResourceName : String) : String;
function FormatFileSize(const ASize : Int64) : string;

procedure InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo; const ALargeIcon : Boolean = False);
function SystemFileIcon(const AFileName : string; AExtensionMode : Boolean = False) : Integer;
function SystemFolderIcon(APath : String = '') : Integer;
function GetWindowsDirectory() : string;

procedure Open(const ACommand : String);

procedure CheckValidIpV4Address(const AValue : String);
function TryCheckValidIpV4Address(const AValue : String) : Boolean;
procedure CheckCertificateFingerprint(const AValue : String);

function CompareObjectAssigmenet(const AObject1, AObject2 : TObject) : Integer;
function CompareIPv4(const AIp1, AIp2 : String) : Integer;

implementation

uses Winapi.Windows, System.IOUtils;

{ _.CompareIPv4 }
function CompareIPv4(const AIp1, AIp2 : String) : Integer;
begin
  if not TryCheckValidIpV4Address(AIp1) or not TryCheckValidIpV4Address(AIp2) then
    Result := CompareText(AIp1, AIp2)
  else begin
    var Parts1 := AIp1.Split(['.']);
    var Parts2 := AIp2.Split(['.']);

    for var I := 0 to 3 do begin
      var AValue1 := StrToIntDef(Parts1[I], 0);
      var AValue2 := StrToIntDef(Parts2[I], 0);

      if AValue1 < AValue2 then
        Exit(-1)
      else if AValue1 > AValue2 then
        Exit(1);
    end;

    ///
    Result := 0;
  end;
end;


{ _.CompareObjectAssignement }
function CompareObjectAssigmenet(const AObject1, AObject2 : TObject) : Integer;
begin
  if not Assigned(AObject1) and not Assigned(AObject2) then
    Result := 0
  else if not Assigned(AObject1) then
    Result := 1
  else if not Assigned(AObject2) then
    Result := -1
end;

{ _.CheckCertificateFingerprint }
procedure CheckCertificateFingerprint(const AValue : String);
begin
  if not TRegEx.IsMatch(AValue, '^([0-9A-Fa-f]{2}:){63}[0-9A-Fa-f]{2}$') then
    raise Exception.Create(
      'Invalid certificate fingerprint. It must be a valid SHA-512 fingerprint, with each byte separated by a colon ' +
      '(e.g., AA:BB:CC:DD…:FF).'
    );
end;

{ _.IsValidIpV4Address }
procedure CheckValidIpV4Address(const AValue : String);
begin
  if not TRegEx.IsMatch(AValue, '^(?:(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d).){3}(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)$') then
    raise Exception.Create('Invalid IPv4 address format. Please enter a valid IPv4 address (e.g., 192.168.0.1).');
end;

{ _.TryCheckValidIpV4Address }
function TryCheckValidIpV4Address(const AValue : String) : Boolean;
begin
  try
    CheckValidIpV4Address(AValue);

    ///
    result := True;
  except
    result := False;
  end;
end;

{ _.Open }
procedure Open(const ACommand : String);
begin
  ShellExecute(0, 'open', PWideChar(ACommand), nil, nil, SW_SHOW);
end;

{ _.GetWindowsDirectory }
function GetWindowsDirectory() : string;
begin
  SetLength(result, MAX_PATH);

  var ALen := WinAPI.Windows.GetWindowsDirectory(@result[1], MAX_PATH);

  SetLength(result, ALen);
  if ALen > MAX_PATH then
    WinAPI.Windows.GetWindowsDirectory(@result[1], ALen);

  ///
  result := IncludeTrailingPathDelimiter(result);
end;

{ _.InitializeSystemIcons }
procedure InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo; const ALargeIcon : Boolean = False);
var AFlags : Integer;
begin
  ZeroMemory(@AFileInfo, SizeOf(TSHFileInfo));
  ///

  if ALargeIcon then
    AFlags := SHGFI_LARGEICON
  else
    AFlags := SHGFI_SMALLICON;

  AImages.Handle := SHGetFileInfo(
                                    PChar(TPath.GetPathRoot(GetWindowsDirectory())),
                                    0,
                                    AFileInfo,
                                    SizeOf(AFileInfo),
                                    AFlags or (SHGFI_SYSICONINDEX)
  );
end;

{ _.SystemFileIcon }
function SystemFileIcon(const AFileName : string; AExtensionMode : Boolean = False) : Integer;
var AFileInfo : TSHFileInfo;
begin
  ZeroMemory(@AFileInfo, sizeof(AFileInfo));
  ///

  if not AExtensionMode then
    AExtensionMode := not FileExists(AFileName);

  var AFlags := SHGFI_SMALLICON or SHGFI_SYSICONINDEX;
  if AExtensionMode then
    AFlags := AFlags or SHGFI_USEFILEATTRIBUTES;

  SHGetFileInfo(PWideChar(AFileName), 0, AFileInfo, SizeOf(AFileInfo), AFlags);

  Result := AFileInfo.iIcon;
end;

{ _.SystemFolderIcon }
function SystemFolderIcon(APath : String = '') : Integer;
var AFileInfo : TSHFileInfo;
begin
  ZeroMemory(@AFileInfo, sizeof(AFileInfo));
  ///

  if APath = '' then
    APath := GetWindowsDirectory();

  var AFlags := SHGFI_SYSICONINDEX;

  SHGetFileInfo(PChar(APath), 0, AFileInfo, SizeOf(AFileInfo), AFlags);

  Result := AFileInfo.iIcon;
end;

{ _.FormatFileSize }
function FormatFileSize(const ASize : Int64) : string;
const AByteDescription : array[0..9-1] of string = (
  'Bytes', 'KiB', 'MB', 'GiB', 'TB',
  'PB', 'EB', 'ZB', 'YB'
);
begin
  var ACount := 0;

  while ASize > Power(1024, ACount +1) do
    Inc(ACount);

  ///
  result := Format('%s %s', [
    FormatFloat('###0.00', ASize / Power(1024, ACount)),
    AByteDescription[ACount]]
  );
end;

{ _.ReadResourceString }
function ReadResourceString(const AResourceName : String) : String;
begin
  var AResourceStream := TResourceStream.Create(hInstance, AResourceName, RT_RCDATA);
  try
    SetString(result, PAnsiChar(AResourceStream.Memory), AResourceStream.Size);
  finally
    FreeAndNil(AResourceStream);
  end;
end;

{ _.TryReadResourceString }
function TryReadResourceString(const AResourceName : String) : String;
begin
  try
    result := ReadResourceString(AResourceName);
  except
    result := '';
  end;
end;

{ _.DefaultIfEmpty }
function DefaultIfEmpty(const AValue : String; const ADefault : String = '-') : String;
begin
  if String.IsNullOrEmpty(AValue) then
    result := ADefault
  else
    result := AValue;
end;

{ _.FormatInt }
function FormatInt(const AInteger : Integer) : String;
begin
  result := Format('%d (0x%p)', [
    AInteger,
    Pointer(AInteger)
  ]);
end;

{ _.ElapsedTime }
function ElapsedTime(const ADays, AHours, AMinutes, ASeconds : UInt64) : String;
begin
  if ADays > 0 then
    result := Format('%d days, %.2d:%.2d:%.2d', [
      ADays,
      AHours,
      AMinutes,
      ASeconds
    ])
  else if AHours > 0 then
    result := Format('%.2d:%.2d:%.2d', [
      AHours,
      AMinutes,
      ASeconds
    ])
  else if AMinutes > 0 then
    result := Format('%d minutes and %.2d seconds.', [
      AMinutes,
      ASeconds
    ])
  else if ASeconds >= 0 then
    result := Format('%d seconds ago.', [ASeconds]);
end;

{ _.ElapsedTime }
function ElapsedTime(const AMilliseconds : UInt64) : String;
var ASpan : TTimeSpan;
begin
  ASpan := TTimeSpan.FromMilliseconds(AMilliseconds);
  ///

  result := ElapsedTime(
    ASpan.Days,
    ASpan.Hours,
    ASpan.Minutes,
    ASpan.Seconds
  );
end;

{ _.ElapsedDateTime }
function ElapsedDateTime(const AFirstDateTime, ASecondDateTime : TDateTime) : String;
var AElaspedTime  : TDateTime;
    ADays         : Integer;
    AHours        : Word;
    AMinutes      : Word;
    ASeconds      : Word;
    AMilliSeconds : Word;
begin
  AElaspedTime := ASecondDateTime - AFirstDateTime;
  ///

  ADays := DaysBetween(ASecondDateTime, AFirstDateTime);

  DecodeTime(AElaspedTime, AHours, AMinutes, ASeconds, AMilliSeconds);

  ///
  result := ElapsedTime(ADays, AHours, AMinutes, ASeconds);
end;

end.
