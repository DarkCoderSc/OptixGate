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

unit __uBaseFormControl__;

interface

uses VCL.Forms, VCL.Controls, System.Classes, Winapi.Messages, XSuperObject, Optix.Func.Commands, Generics.Collections;

type
  TFormControlState = (
    fcsUnset,
    fcsVisible,
    fcsMinimized,
    fcsClosed
  );

  TFormControlInformation = class(TPersistent)
  private
    FGUID                 : TGUID;
    FUserIdentifier       : String;
    FCreatedTime          : TDateTime;
    FHasReceivedData      : Boolean;
    FLastReceivedDataTime : TDateTime;
    FHasUnseenData        : Boolean;
    FHasFocus             : Boolean;
    FState                : TFormControlState;

    {@M}
    procedure SetHasFocus(const AValue : Boolean);
    procedure SetHasUnseenData(const AValue : Boolean);
  public
    {@C}
    constructor Create();

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property GUID                 : TGUID     read FGUID;
    property CreatedTime          : TDateTime read FCreatedTime;

    {@G/S}
    property LastReceivedDataTime : TDateTime         read FLastReceivedDataTime write FLastReceivedDataTime;
    property HasReceivedData      : Boolean           read FHasReceivedData      write FHasReceivedData;
    property HasUnseenData        : Boolean           read FHasUnseenData        write SetHasUnseenData;
    property UserIdentifier       : String            read FUserIdentifier       write FUserIdentifier;
    property HasFocus             : Boolean           read FHasFocus             write SetHasFocus;
    property State                : TFormControlState read FState                write FState;
  end;

  TBaseFormControl = class(TForm)
  private
    FOriginalCaption  : String;
    FDialogs          : TObjectList<TForm>;
    FFirstShow        : Boolean;

    {@M}
    function GetGUID() : TGUID;
  protected
    FSpecialForm     : Boolean;
    FFormInformation : TFormControlInformation;

    {@M}
    function GetContextDescription() : String; virtual;
    procedure RefreshCaption(); virtual;

    procedure RegisterNewDialogAndShow(const ADialog : TForm);

    function RequestFileDownload(ARemoteFilePath : String = ''; ALocalFilePath : String = ''; const AContext : String = '') : TGUID; virtual;
    function RequestFileUpload(ALocalFilePath : String = ''; ARemoteFilePath : String = ''; const AContext : String = '') : TGUID; virtual;

    procedure DoShow(); override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure OnFirstShow(); virtual;

    procedure CMVisibleChanged(var AMessage: TMessage); message CM_VISIBLECHANGED;
    procedure WMActivateApp(var AMessage: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure CMActivate(var AMessage: TCMActivate); message CM_ACTIVATE;
    procedure CMDeactivate(var AMessage: TCMDeactivate); message CM_DEACTIVATE;
    procedure WMWindowPosChanging(var AMessage: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  public
    {@M}
    procedure SendCommand(const ACommand : TOptixCommand);
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); virtual;
    procedure PurgeRequest(); virtual;

    {@C}
    constructor Create(AOwner : TComponent; const AUserIdentifier : String; const ASpecialForm : Boolean = False); reintroduce; virtual;
    destructor Destroy(); override;

    {@G}
    property GUID               : TGUID                   read GetGUID;
    property SpecialForm        : Boolean                 read FSpecialForm;
    property FormInformation    : TFormControlInformation read FFormInformation;
    property ContextInformation : String                  read GetContextDescription;
  end;

  TBaseFormControlClass = class of TBaseFormControl;

  function FormControlStateToString(const AValue : TFormControlState) : String;

implementation

uses System.SysUtils, Winapi.Windows, uFormMain, uControlFormTransfers;

(* Local *)

function FormControlStateToString(const AValue : TFormControlState) : String;
begin
  case AValue of
    fcsVisible   : result := 'Visible';
    fcsMinimized : result := 'Minimized';
    fcsClosed    : result := 'Closed';

    else
      result := 'Unset';
  end;
end;

(* TFormControlInformation *)

{ TFormControlInformation.Create }
constructor TFormControlInformation.Create();
begin
  inherited;
  ///

  FGUID            := TGUID.NewGuid();
  FUserIdentifier  := '';
  FCreatedTime     := Now;
  FHasReceivedData := False;
  FHasUnseenData   := False;
  FState           := fcsUnset;
end;

{ TFormControlInformation.Assign }
procedure TFormControlInformation.Assign(ASource : TPersistent);
begin
  if ASource is TFormControlInformation then begin
    FGUID                 := TFormControlInformation(ASource).FGUID;
    FUserIdentifier       := TFormControlInformation(ASource).FUserIdentifier;
    FCreatedTime          := TFormControlInformation(ASource).FCreatedTime;
    FHasReceivedData      := TFormControlInformation(ASource).FHasReceivedData;
    FLastReceivedDataTime := TFormControlInformation(ASource).FLastReceivedDataTime;
    FHasUnseenData        := TFormControlInformation(ASource).FHasUnseenData;
    FHasFocus             := TFormControlInformation(ASource).FHasFocus;
    FState                := TFormControlInformation(ASource).FState;
  end else
    inherited;
end;

{ TBaseFormControl.SetHasFocus }
procedure TFormControlInformation.SetHasFocus(const AValue : Boolean);
begin
  if AValue then
    FHasUnseenData := False;
  ///

  FHasFocus := AValue;
end;

{ TBaseFormControl.SetHasUnseenData }
procedure TFormControlInformation.SetHasUnseenData(const AValue : Boolean);
begin
  if AValue then
    FHasUnseenData := not FHasFocus
  else
    FHasUnseenData := AValue;
end;

(* TBaseFormControl *)

procedure TBaseFormControl.RegisterNewDialogAndShow(const ADialog : TForm);
begin
  if Assigned(ADialog) and Assigned(FDialogs) then begin
    FDialogs.Add(ADialog);

    ///
    ADialog.Show();
  end;
end;

function TBaseFormControl.GetContextDescription() : String;
begin
  result := '';
  ///
end;

{ TBaseFormControl.CreateParams }
procedure TBaseFormControl.ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject);
begin
  FFormInformation.HasUnseenData        := True;
  FFormInformation.HasReceivedData      := True;
  FFormInformation.LastReceivedDataTime := Now;
  ///

end;

{ TBaseFormControl.CreateParams }
procedure TBaseFormControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  Params.ExStyle := Params.ExStyle and NOT WS_EX_APPWINDOW;

  Params.WndParent := 0;
end;

{ TBaseFormControl.DoShow }
procedure TBaseFormControl.DoShow();
begin
  inherited;
  ///

  if FFirstShow then begin
    FFirstShow := False;

    ///
    OnFirstShow();
  end;
end;

{ TBaseFormControl.OnFirstShow }
procedure TBaseFormControl.OnFirstShow();
begin
  ///
end;

{ TBaseFormControl.Create }
constructor TBaseFormControl.Create(AOwner : TComponent; const AUserIdentifier : String; const ASpecialForm : Boolean = False);
begin
  inherited Create(AOwner);
  ///

  FOriginalCaption := self.Caption; // Default
  FSpecialForm     := ASpecialForm;
  FFirstShow       := True;
  FFormInformation := TFormControlInformation.Create();

  FFormInformation.UserIdentifier := AUserIdentifier;
  FFormInformation.State := fcsClosed;

  FDialogs := TObjectList<TForm>.Create(True);
end;

{ TBaseFormControl.Destroy }
destructor TBaseFormControl.Destroy();
begin
  if Assigned(FFormInformation) then
    FreeAndNil(FFormInformation);

  if Assigned(FDialogs) then
    FreeAndNil(FDialogs);

  ///
  inherited;
end;

{ TBaseFormControl.SendCommand }
procedure TBaseFormControl.SendCommand(const ACommand : TOptixCommand);
begin
  ACommand.WindowGUID := FFormInformation.GUID;

  ///
  FormMain.SendCommand(self, ACommand);
end;

{ TBaseFormControl.RequestFileDownload }
function TBaseFormControl.RequestFileDownload(ARemoteFilePath : String = ''; ALocalFilePath : String = ''; const AContext : String = '') : TGUID;
begin
  var AForm := FormMain.GetControlForm(self, TControlFormTransfers);
  if Assigned(AForm) then
    AForm.RequestFileDownload(ARemoteFilePath, ALocalFilePath, AContext);
end;

{ TBaseFormControl.RequestFileUpload }
function TBaseFormControl.RequestFileUpload(ALocalFilePath : String = ''; ARemoteFilePath : String = ''; const AContext : String = '') : TGUID;
begin
  var AForm := FormMain.GetControlForm(self, TControlFormTransfers);
  if Assigned(AForm) then
    AForm.RequestFileUpload(ALocalFilePath, ARemoteFilePath, AContext);
end;

{ TBaseFormControl.CMVisibleChanged }
procedure TBaseFormControl.CMVisibleChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  if self.Visible then
    RefreshCaption();

  if not Assigned(FFormInformation) then
    Exit();

  if self.Visible then
    FFormInformation.State := fcsVisible
  else
    FFormInformation.State := fcsClosed;
end;

{ TBaseFormControl.RefreshCaption }
procedure TBaseFormControl.RefreshCaption();
begin
  if String.IsNullOrEmpty(FFormInformation.UserIdentifier) then
    Caption := FOriginalCaption
  else
    Caption := Format('%s (%s)', [
      FOriginalCaption,
      FFormINformation.UserIdentifier
    ]);
end;

{ TBaseFormControl.GetGUID }
function TBaseFormControl.GetGUID() : TGUID;
begin
  result := FFormInformation.GUID
end;

{ TBaseFormControl.WMActivateApp }
procedure TBaseFormControl.WMActivateApp(var AMessage: TWMActivateApp);
begin
  inherited;

  if not Assigned(FFormInformation) then
    Exit();

  var AIsActive := AMessage.Active;

  if AMessage.Active and (self.Handle <> GetForegroundWindow()) then
    AIsActive := False;


  ///
  FFormInformation.SetHasFocus(AIsActive);
end;

{ TBaseFormControl.CMActivate }
procedure TBaseFormControl.CMActivate(var AMessage: TCMActivate);
begin
  inherited;
  ///

  if not Assigned(FFormInformation) then
    Exit();

  ///
  FFormInformation.SetHasFocus(True);
end;

{ TBaseFormControl.CMDeactivate }
procedure TBaseFormControl.CMDeactivate(var AMessage: TCMDeactivate);
begin
  inherited;
  ///

  if not Assigned(FFormInformation) then
    Exit();

  ///
  FFormInformation.SetHasFocus(False);
end;

{ TBaseFormControl.WMSize }
procedure TBaseFormControl.WMWindowPosChanging(var AMessage: TWMWindowPosChanging);
begin
  inherited;
  ///

  if not Assigned(FFormInformation) then
    Exit();

  case WindowState of
    TWindowState.wsNormal    : FFormInformation.State := fcsVisible;
    TWindowState.wsMinimized : FFormInformation.State := fcsMinimized;
    TWindowState.wsMaximized : ;
  end;
end;

{ TBaseFormControl.PurgeRequest }
procedure TBaseFormControl.PurgeRequest();
begin
  ///
end;

end.
