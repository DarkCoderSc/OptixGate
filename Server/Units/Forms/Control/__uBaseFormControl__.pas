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
{  Authorship (No AI):                                                         }
{  -------------------                                                         }
{   All code contained in this unit was written and developed by the author    }
{   without the assistance of artificial intelligence systems, large language  }
{   models (LLMs), or automated code generation tools. Any external libraries  }
{   or frameworks used comply with their respective licenses.	                 }
{                                                                              }
{******************************************************************************}

unit __uBaseFormControl__;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Generics.Collections,

  Winapi.Messages,

  VCL.Forms, VCL.Controls, VCL.Menus,

  OptixCore.Commands.Base, OptixCore.Protocol.Packet,

  NeoFlat.Form, NeoFlat.CaptionBar;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFormControlState = (
    fcsUnset,
    fcsVisible,
    fcsMinimized,
    fcsClosed,
    fcsWaitFree
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
    procedure SetState(const AValue : TFormControlState);
  public
    {@C}
    constructor Create();

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property CreatedTime : TDateTime read FCreatedTime;

    {@G/S}
    property LastReceivedDataTime : TDateTime         read FLastReceivedDataTime write FLastReceivedDataTime;
    property HasReceivedData      : Boolean           read FHasReceivedData      write FHasReceivedData;
    property HasUnseenData        : Boolean           read FHasUnseenData        write SetHasUnseenData;
    property UserIdentifier       : String            read FUserIdentifier       write FUserIdentifier;
    property HasFocus             : Boolean           read FHasFocus             write SetHasFocus;
    property State                : TFormControlState read FState                write SetState;
    property GUID                 : TGUID             read FGUID                 write FGUID;
  end;

  TBaseFormControl = class(TForm)
  private
    FOriginalCaption : String;
    FFlatForm        : TFlatForm;
    FFirstShow       : Boolean;

    {@M}
    function GetGUID() : TGUID;
  protected
    FCaptionBar      : TFlatCaptionBar;
    FSpecialForm     : Boolean;
    FFormInformation : TFormControlInformation;

    {@M}
    function GetContextDescription() : String; virtual;
    procedure RefreshCaption(); virtual;

    function RequestFileDownload(ARemoteFilePath : String = ''; ALocalFilePath : String = '';
      const AContext : String = '') : TGUID; virtual;
    function RequestFileUpload(ALocalFilePath : String = ''; ARemoteFilePath : String = '';
      const AContext : String = '') : TGUID; virtual;

    procedure StreamFileContent(const AFilePath : String; const APageSize : UInt64 = 1024);

    procedure DoShow(); override;
    procedure DoClose(var Action: TCloseAction); override;

    procedure CreateParams(var Params: TCreateParams); override;

    procedure OnFirstShow(); virtual;

    procedure CMVisibleChanged(var AMessage: TMessage); message CM_VISIBLECHANGED;
    procedure WMActivateApp(var AMessage: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure CMActivate(var AMessage: TCMActivate); message CM_ACTIVATE;
    procedure CMDeactivate(var AMessage: TCMDeactivate); message CM_DEACTIVATE;
    procedure WMWindowPosChanging(var AMessage: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;

    procedure SetCaptionBarDropDown(const AValue : TPopupMenu);
  public
    {@M}
    procedure SendCommand(const ACommand : TOptixCommand); overload;
    procedure SendCommand(const ACommand : TOptixCommand; const AControlFormGUIDForCallBack : TGUID); overload;
    procedure ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean); virtual;
    procedure PurgeRequest(); virtual;
    procedure __WARNING__OverrideWindowGUID(const ANewGUID : TGUID); (* Warning | TODO: Safer method *)

    {@C}
    constructor Create(AOwner : TComponent; const AUserIdentifier : String;
      const ASpecialForm : Boolean = False); reintroduce; virtual;
    destructor Destroy(); override;

    {@G}
    property GUID                : TGUID                   read GetGUID;
    property SpecialForm         : Boolean                 read FSpecialForm;
    property FormInformation     : TFormControlInformation read FFormInformation;
    property ContextInformation  : String                  read GetContextDescription;

    {@S}
    property CaptionBarDropDown : TPopupMenu write SetCaptionBarDropDown;
  end;

  TBaseFormControlClass = class of TBaseFormControl;

  function FormControlStateToString(const AValue : TFormControlState) : String;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Winapi.Windows,

  OptixCore.Commands.ContentReader,

  uFormMain, uControlFormTransfers;
// ---------------------------------------------------------------------------------------------------------------------

(* Local *)

function FormControlStateToString(const AValue : TFormControlState) : String;
begin
  case AValue of
    fcsVisible   : result := 'Visible';
    fcsMinimized : result := 'Minimized';
    fcsClosed    : result := 'Closed';
    fcsWaitFree  : result := 'Wait Free';

    else
      result := 'Unset';
  end;
end;

(* TFormControlInformation *)

constructor TFormControlInformation.Create();
begin
  inherited Create();
  ///

  FGUID            := TGUID.NewGuid;
  FUserIdentifier  := '';
  FCreatedTime     := Now;
  FHasReceivedData := False;
  FHasUnseenData   := False;
  FState           := fcsUnset;
end;

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

procedure TFormControlInformation.SetHasFocus(const AValue : Boolean);
begin
  if AValue then
    FHasUnseenData := False;
  ///

  FHasFocus := AValue;
end;

procedure TFormControlInformation.SetHasUnseenData(const AValue : Boolean);
begin
  if AValue then
    FHasUnseenData := not FHasFocus
  else
    FHasUnseenData := AValue;
end;

procedure TFormControlInformation.SetState(const AValue : TFormControlState);
begin
  if (FState = AValue) or (FState = fcsWaitFree) then
    Exit();

  ///
  FState := AValue;
end;

(* TBaseFormControl *)

function TBaseFormControl.GetContextDescription() : String;
begin
  result := '';
  ///
end;

procedure TBaseFormControl.ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean);
begin
  if not Assigned(AOptixPacket) then
    Exit();
  ///

  AHandleMemory := False;

  FFormInformation.HasUnseenData        := True;
  FFormInformation.HasReceivedData      := True;
  FFormInformation.LastReceivedDataTime := Now;
end;

procedure TBaseFormControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  Params.ExStyle := Params.ExStyle and NOT WS_EX_APPWINDOW;

  Params.WndParent := 0;
end;

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

procedure TBaseFormControl.DoClose(var Action: TCloseAction);
begin
  inherited;
  ///

  if Action = caFree then begin
    Action := caHide;

    ///
    FFormInformation.State := fcsWaitFree;
  end;
end;

procedure TBaseFormControl.OnFirstShow();
begin
  Height := Height +1;
end;

constructor TBaseFormControl.Create(AOwner : TComponent; const AUserIdentifier : String;
  const ASpecialForm : Boolean = False);
begin
  inherited Create(AOwner);
  ///

  FFlatForm := TFlatForm.Create(self);

  FCaptionBar := TFlatCaptionBar.Create(self);
  FCaptionBar.Form := FFlatForm;
  FCaptionBar.Parent := self;
  FCaptionBar.BorderIcons := BorderIcons;

  FOriginalCaption := Caption; // Default
  FSpecialForm     := ASpecialForm;
  FFirstShow       := True;
  FFormInformation := TFormControlInformation.Create();

  FFormInformation.UserIdentifier := AUserIdentifier;
  FFormInformation.State := fcsClosed;
end;

destructor TBaseFormControl.Destroy();
begin
  if Assigned(FFormInformation) then
    FreeAndNil(FFormInformation);

  if Assigned(FCaptionBar) then
    FreeAndNil(FCaptionBar);

  if Assigned(FFlatForm) then
    FreeAndNil(FFlatForm);

  ///
  inherited;
end;

procedure TBaseFormControl.SendCommand(const ACommand : TOptixCommand);
begin
  ACommand.WindowGUID := FFormInformation.GUID;

  ///
  FormMain.SendCommand(self, ACommand);
end;

procedure TBaseFormControl.SendCommand(const ACommand : TOptixCommand; const AControlFormGUIDForCallBack : TGUID);
begin
  ACommand.WindowGUID := AControlFormGUIDForCallBack;

  ///
  FormMain.SendCommand(self, ACommand);
end;

function TBaseFormControl.RequestFileDownload(ARemoteFilePath : String = ''; ALocalFilePath : String = ''; const AContext : String = '') : TGUID;
begin
  var AForm := FormMain.GetControlForm(self, TControlFormTransfers);
  if Assigned(AForm) then
    AForm.RequestFileDownload(ARemoteFilePath, ALocalFilePath, AContext);
end;

function TBaseFormControl.RequestFileUpload(ALocalFilePath : String = ''; ARemoteFilePath : String = ''; const AContext : String = '') : TGUID;
begin
  var AForm := FormMain.GetControlForm(self, TControlFormTransfers);
  if Assigned(AForm) then
    AForm.RequestFileUpload(ALocalFilePath, ARemoteFilePath, AContext);
end;

procedure TBaseFormControl.StreamFileContent(const AFilePath : String; const APageSize : UInt64 = 1024);
begin
  if String.IsNullOrWhiteSpace(AFilePath) then
    Exit();
  ///

  SendCommand(TOptixCommandCreateFileContentReader.Create(AFilePath, APageSize));
end;

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

procedure TBaseFormControl.RefreshCaption();
begin
  if String.IsNullOrEmpty(FFormInformation.UserIdentifier) then
    Caption := FOriginalCaption
  else
    Caption := Format('%s (%s)', [
      FOriginalCaption,
      FFormINformation.UserIdentifier
    ]);

  ///
  if Assigned(FCaptionBar) then
    FCaptionBar.Caption := Caption;
end;

function TBaseFormControl.GetGUID() : TGUID;
begin
  result := FFormInformation.GUID
end;

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

procedure TBaseFormControl.CMActivate(var AMessage: TCMActivate);
begin
  inherited;
  ///

  if not Assigned(FFormInformation) then
    Exit();

  ///
  FFormInformation.SetHasFocus(True);
end;

procedure TBaseFormControl.CMDeactivate(var AMessage: TCMDeactivate);
begin
  inherited;
  ///

  if not Assigned(FFormInformation) then
    Exit();

  ///
  FFormInformation.SetHasFocus(False);
end;

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

procedure TBaseFormControl.PurgeRequest();
begin
  ///
end;

procedure TBaseFormControl.__WARNING__OverrideWindowGUID(const ANewGUID : TGUID);
begin
  FFormInformation.GUID := ANewGUID;
end;

procedure TBaseFormControl.SetCaptionBarDropDown(const AValue : TPopupMenu);
begin
  if Assigned(FCaptionBar) then
    FCaptionBar.MenuDropDown := AValue;
end;

end.
