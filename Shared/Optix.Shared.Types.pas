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
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{    I dedicate this work to my daughter & wife                                }
{                                                                              }
{******************************************************************************}

unit Optix.Shared.Types;

interface

uses System.SysUtils, System.Classes, System.Hash, Winapi.Windows;

type
  TBeaconId  = record
    Id         : TGUID;
    ServerPort : Word;

    {@C}
    class function Create(const AId : TGUID; const AServerPort : Word): TBeaconId; static;

    {@O}
    class operator Equal(Left, Right: TBeaconId): Boolean; inline;
    class operator NotEqual(Left, Right: TBeaconId): Boolean; inline;

    {@M}
    class function Empty: TBeaconId; static;
    function ToString: String;
    function IsEmpty: Boolean;
  end;

  ESessionFormatException = class(Exception);
  ESessionLengthException = class(ESessionFormatException);
  ESessionDataException = class(ESessionFormatException);

  TSessionId = record
  private
    Data : array[0..64-1] of byte;
  public
    {@C}
    class function Create(): TSessionId; overload; static;
    class function Create(const ASessionString : String) : TSessionId; overload; static;

    {@O}
    class operator Equal(Left, Right: TSessionId): Boolean; inline;
    class operator NotEqual(Left, Right: TSessionId): Boolean; inline;

    {@M}
    class function Empty: TSessionId; static;
    function ToString: String;
    function IsEmpty: Boolean;
  end;

implementation

(* TBeaconId *)

{ TBeaconId.Create }
class function TBeaconId.Create(const AId : TGUID; const AServerPort : Word): TBeaconId;
begin
  result.Id := AId;
  result.ServerPort := AServerPort;
end;

{ TBeaconId.Equal }
class operator TBeaconId.Equal(Left, Right: TBeaconId): Boolean;
begin
  result := (Left.Id = Right.Id) and (Left.ServerPort = Right.ServerPort);
end;

{ TBeaconId.NotEqual }
class operator TBeaconId.NotEqual(Left, Right: TBeaconId): Boolean;
begin
  result := not (Left = Right);
end;

{ TBeaconId.Empty }
class function TBeaconId.Empty: TBeaconId;
begin
  result := TBeaconId.Create(TGUID.Empty, 0);
end;

{ TBeaconId.ToString }
function TBeaconId.ToString: String;
begin
  result := Format('%s:{%d}', [Id.ToString, ServerPort]);
end;

{ TBeaconId.IsEmpty }
function TBeaconId.IsEmpty: Boolean;
begin
  result := Id.IsEmpty;
end;

(* TSessionId *)

{ TSessionId.Creatge }
class function TSessionId.Create(): TSessionId;
begin
  // Instead of relying on `THash.GetRandomString()`, we could use OpenSSL to generate
  // cryptographically secure random values; however, this is not necessary in
  // this case, as generating non-cryptographically secure random data has no
  // security implications for session id purpose and implementation.
  var ACandidate := THashSHA2.GetHashBytes(
    THash.GetRandomString(128),
    THashSHA2.TSHA2Version.SHA512
  );

  CopyMemory(@result.Data[0], @ACandidate[0], Length(result.Data));
end;

{ TSessionId.Create }
class function TSessionId.Create(const ASessionString : String) : TSessionId;
begin
  if Length(ASessionString) <> 128 then
    raise ESessionLengthException.Create('The session string must be exactly 128 characters long.');

  if HexToBin(PWideChar(ASessionString), result.Data, Length(result.Data)) <> Length(result.Data) then
    raise ESessionDataException.Create('The session string must consist of a valid sequence of hexadecimal characters.');
end;

{ TSessionId.Equal }
class operator TSessionId.Equal(Left, Right: TSessionId): Boolean;
begin
  result := CompareMem(@Left.Data[0], @Right.Data[0], Length(Left.Data));
end;

{ TSessionId.NotEqual }
class operator TSessionId.NotEqual(Left, Right: TSessionId): Boolean;
begin
  result := not (Left = Right);
end;

{ TSessionId.ToString }
function TSessionId.ToString: String;
begin
  SetLength(result, Length(Data) * 2);

  BinToHex(Data, PWideChar(result), Length(Data));
end;

{ TSessionId.Empty }
class function TSessionId.Empty: TSessionId;
begin
  ZeroMemory(@result.Data[0], Length(result.Data));
end;

{ TSessionId.IsEmpty }
function TSessionId.IsEmpty: Boolean;
begin
  var AZeroBlock : array of byte;
  SetLength(AZeroBlock, Length(Data));

  // Although SetLength guarantees zero-initialized memory in modern Delphi versions,
  // earlier versions did not provide this assurance. To maintain compatibility and
  // ensure predictable behavior, I explicitly zero out the array using ZeroMemory
  // or FillChar.
  ZeroMemory(@AZeroBlock[0], Length(AZeroBlock));

  result := CompareMem(@AZeroBlock[0], @Data[0], Length(Data));
end;

end.
