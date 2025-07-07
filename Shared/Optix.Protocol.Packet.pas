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

unit Optix.Protocol.Packet;

interface

uses System.Classes, System.SysUtils, Optix.Interfaces, XSuperObject;

{$I Optix.inc}

type
  TOptixPacket = class(TInterfacedPersistent, IOptixSerializable)
  private
    FSessionId  : TGUID;
    FWindowGUID : TGUID;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); virtual;
    procedure BeforeCreate(); virtual;
  public
    {@M}
    function Serialize() : ISuperObject; virtual;

    {@C}
    constructor Create(); overload; virtual;
    constructor Create(const ASerializedObject : ISuperObject); overload; virtual;
    constructor Create(const AWindowGUID : TGUID); overload; virtual;

    {@G/S}
    property WindowGUID : TGUID read FWindowGUID write FWindowGUID;

    {@G}
    property SessionId : TGUID read FSessionId;
  end;

  {$IFDEF CLIENT}
  var SESSION_ID : TGUID;
  {$ENDIF}

implementation

uses Optix.InformationGathering.Helper;

(* TOptixPacket *)

{ TOptixPacket.Create }
constructor TOptixPacket.Create();
begin
  BeforeCreate();
  ///

  inherited;

  FWindowGUID := TGUID.Empty;

  {$IFDEF CLIENT}
  FSessionId := SESSION_ID;
  {$ELSE}
  FSessionId := TGUID.Empty();
  {$ENDIF}
end;

{ TOptixPacket.Create }
constructor TOptixPacket.Create(const ASerializedObject : ISuperObject);
begin
  BeforeCreate();
  ///

  Create();
  ///

  if Assigned(ASerializedObject) then
    DeSerialize(ASerializedObject)
end;

{ TOptixPacket.Create }
constructor TOptixPacket.Create(const AWindowGUID : TGUID);
begin
  Create();
  ///

  FWindowGUID := AWindowGUID;
end;

{ TOptixPacket.Serialize }
function TOptixPacket.Serialize() : ISuperObject;
begin
  result := TSuperObject.Create();
  ///

  result.S['PacketClass'] := self.ClassName;
  result.S['SessionId']   := FSessionId.ToString();
  result.S['WindowGUID']  := FWindowGUID.ToString();
end;

{ TOptixPacket.DeSerialize }
procedure TOptixPacket.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    Exit();
  ///

  {$IFDEF SERVER}
  FSessionId := TGUID.Create(ASerializedObject.S['SessionId']);
  {$ENDIF}

  FWindowGUID := TGUID.Create(ASerializedObject.S['WindowGUID']);
end;

{ TOptixPacket.BeforeCreate }
procedure TOptixPacket.BeforeCreate();
begin
  ///
end;

(* __INIT__ *)

initialization
  {$IFDEF CLIENT}
  SESSION_ID := TOptixInformationGathering.GetUserUID();
  {$ENDIF}

end.
