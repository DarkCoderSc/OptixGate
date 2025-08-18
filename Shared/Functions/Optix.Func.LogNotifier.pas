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
{                                                                              }
{******************************************************************************}

unit Optix.Func.LogNotifier;

interface

uses Optix.Protocol.Packet, System.SysUtils, XSuperObject;

type
  TLogKind = (
    lkInformation,
    lkException
    (* ... *)
  );

  TLogNotifier = class(TOptixPacket)
  private
    FMessage    : String;
    FContext    : String;
    FKind       : TLogKind;
  protected
    {@M}
    function GetDetailedMessage() : String; virtual;
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@C}
    constructor Create(const AMessage : String; const AContext : String; const AKind : TLogKind); overload;

    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property Kind            : TLogKind read FKind;
    property LogMessage      : String   read FMessage;
    property DetailedMessage : String   read GetDetailedMessage;
    property Context         : String   read FContext;
  end;

  TLogTransferException = class(TLogNotifier)
  private
    FTransferId : TGUID;
  protected
    {@M}
    function GetDetailedMessage() : String; override;
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@C}
    constructor Create(const ATransferId : TGUID; const AMessage : String; const AContext : String = ''); overload;

    {@M}
    function Serialize() : ISuperObject; override;

    {@}
    property TransferId : TGUID read FTransferId;
  end;

  function LogKindToString(const AValue : TLogKind) : String;

implementation

(* TLogNotifier *)

{ TLogNotifier.Create }
constructor TLogNotifier.Create(const AMessage : String; const AContext : String; const AKind : TLogKind);
begin
  inherited Create();
  ///

  FKind    := AKind;
  FMessage := AMessage;
  FContext := AContext;
end;

{ TLogNotifier.Serialize }
function TLogNotifier.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.I['Kind']    := Cardinal(FKind);
  result.S['Message'] := FMessage;
  result.S['Context'] := FContext;
end;

{ TLogNotifier.GetDetailedMessage }
function TLogNotifier.GetDetailedMessage() : String;
begin
  result := FMessage;
end;

{ TLogNotifier.DeSerialize }
procedure TLogNotifier.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FKind    := TLogKind(ASerializedObject.I['Kind']);
  FMessage := ASerializedObject.S['Message'];
  FContext := ASerializedObject.S['Context'];
end;

(* TLogKind *)

function LogKindToString(const AValue : TLogKind) : String;
begin
  case AValue of
    lkInformation : result := 'Information';
    lkException   : result := 'Exception';
    else
      result := 'Unknown';
  end;
end;

(* TLogTransferException *)

{ TLogTransferException.Create }
constructor TLogTransferException.Create(const ATransferId : TGUID; const AMessage : String; const AContext : String = '');
begin
  inherited Create(AMessage, AContext, TLogKind.lkException);
  ///

  FTransferId := ATransferId;
end;

{ TLogTransferException.GetDetailedMessage }
function TLogTransferException.GetDetailedMessage() : String;
begin
  result := Format('Transfer Id:[%s] %s', [
    FTransferId.ToString,
    FMessage
  ]);
end;

{ TLogTransferException.DeSerialize }
procedure TLogTransferException.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FTransferId := TGUID.Create(ASerializedObject.S['TransferId']);
end;

{ TLogTransferException.Serialize }
function TLogTransferException.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['TransferId'] := FTransferId.TOString();
end;

end.
