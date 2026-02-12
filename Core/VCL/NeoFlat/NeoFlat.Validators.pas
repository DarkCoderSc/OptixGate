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

unit NeoFlat.Validators;

interface

uses NeoFlat.Types, VCL.Controls, System.Classes, NeoFlat.Panel;

function IsValidIpAddress(const AIP : String) : Boolean;
function IsValidHost(const AHost : String) : Boolean;
function IsValidNetworkAddress(const AValue : String) : Boolean;

function IsValidPort(const APort : Integer) : Boolean; overload;
function IsValidPort(const APort : String) : Boolean; overload

function Validate(const AInput : String; const AValidators : TValidators) : Boolean;

implementation

uses System.RegularExpressions, System.SysUtils, NeoFlat.Edit, NeoFlat.ComboBox, Winapi.Windows;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function Validate(const AInput : String; const AValidators : TValidators) : Boolean;
begin
  result := False;
  ///

  { Filled }
  if reqFilled in AValidators then
    if Length(Trim(AInput)) = 0 then
      Exit();

  { Ip Address }
  if reqIpAddress in AValidators then
    if not IsValidIpAddress(AInput) then
      Exit();

  { Host }
  if reqHost in AValidators then
    if not IsValidHost(AInput) then
      Exit();

  { TCP / UDP Port }
  if reqNetPort in AValidators then
    if not IsValidPort(AInput) then
      Exit();

  ///
  result := True;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidIpAddress(const AIP : String) : Boolean;
begin
  result := TRegEx.IsMatch(AIP, '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$');
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidHost(const AHost : String) : Boolean;
begin
  result := TRegEx.IsMatch(AHost, '^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$');
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidNetworkAddress(const AValue : String) : Boolean;
begin
  result := IsValidIpAddress(AValue) or
            IsValidHost(AValue);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidPort(const APort : Integer) : Boolean;
begin
  result := (APort >= Low(word)) and (APort <= High(word));
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidPort(const APort : String) : Boolean;
var AValue : Integer;
begin
  result := False;
  if not TryStrToInt(APort, AValue) then
    Exit();
  ///

  result := IsValidPort(AValue);
end;

end.
