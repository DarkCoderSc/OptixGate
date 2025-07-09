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

uses System.Classes,
     System.SysUtils,
     System.DateUtils,
     System.Math,
     System.TimeSpan;

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

implementation

uses Winapi.Windows;

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
