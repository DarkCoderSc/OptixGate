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

unit uControlFormRemoteShell;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes, System.Types, System.Actions,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ActnList,

  XSuperObject,

  __uBaseFormControl__, uFrameRemoteShellInstance;
// ---------------------------------------------------------------------------------------------------------------------

type
  TControlFormRemoteShell = class(TBaseFormControl)
    Pages: TPageControl;
    PopupTabs: TPopupMenu;
    erminateInstance1: TMenuItem;
    N1: TMenuItem;
    CloseTabTerminate1: TMenuItem;
    RenameTab1: TMenuItem;
    PanelActions: TPanel;
    ButtonNewInstance: TSpeedButton;
    ActionList: TActionList;
    NewShellInstance1: TAction;
    ButtonBreak: TSpeedButton;
    BreakActiveShellInstance1: TAction;
    procedure PagesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure PopupTabsPopup(Sender: TObject);
    procedure erminateInstance1Click(Sender: TObject);
    procedure CloseTabTerminate1Click(Sender: TObject);
    procedure RenameTab1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonNewInstanceClick(Sender: TObject);
    procedure NewShellInstance1Execute(Sender: TObject);
    procedure ButtonBreakClick(Sender: TObject);
    procedure BreakActiveShellInstance1Execute(Sender: TObject);
    procedure PagesChange(Sender: TObject);
  private
    FHitTab : TTabSheet;

    {@M}
    procedure RequestNewShellInstance();
    function TabNameExists(const AName : String) : Boolean;
    function StartShellInstance(const AInstanceId : TGUID) : TFrameRemoteShellInstance;
    procedure CloseShellInstance(const AInstanceId : TGUID);
    function GetFrameByInstanceId(const AInstanceId : TGUID) : TFrameRemoteShellInstance;
    function GetFrameByTab(const ATab : TTabSheet) : TFrameRemoteShellInstance;
    procedure TerminateShellInstance(const ATab : TTabSheet);
    procedure RefreshActionsButtons();
    function IsActivePageShellInstanceActive() : Boolean;
  public
    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); override;
    procedure PurgeRequest(); override;
  end;

var
  ControlFormRemoteShell: TControlFormRemoteShell;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain,

  Optix.Func.Commands, Optix.Protocol.Packet, Optix.Func.Shell, Optix.Constants, Optix.VCL.Helper,
  Optix.Func.Commands.Shell;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

function TControlFormRemoteShell.IsActivePageShellInstanceActive() : Boolean;
begin
  result := False;

  if Pages.ActivePageIndex <= -1 then
    Exit();

  result := Pages.ActivePage.ImageIndex = -1;
end;

procedure TControlFormRemoteShell.RefreshActionsButtons();
begin
  ButtonBreak.Enabled := IsActivePageShellInstanceActive();
end;

procedure TControlFormRemoteShell.PurgeRequest();
begin
  for var I := 0 to Pages.PageCount -1 do
    TerminateShellInstance(Pages.Pages[I]);
end;

procedure TControlFormRemoteShell.TerminateShellInstance(const ATab : TTabSheet);
begin
  if not Assigned(ATab) then
    Exit();

  var AFrame := GetFrameByTab(ATab);
  if not Assigned(AFrame) then
    Exit();

  SendCommand(TOptixTerminateShellInstance.Create(AFrame.InstanceId));
end;

procedure TControlFormRemoteShell.CloseTabTerminate1Click(Sender: TObject);
begin
  TerminateShellInstance(FHitTab);

  FreeAndNil(FHitTab);

  RefreshActionsButtons();
end;

procedure TControlFormRemoteShell.erminateInstance1Click(Sender: TObject);
begin
  TerminateShellInstance(FHitTab);
end;

procedure TControlFormRemoteShell.FormCreate(Sender: TObject);
begin
  FHitTab := nil;
  RefreshActionsButtons();
end;

procedure TControlFormRemoteShell.FormDestroy(Sender: TObject);
begin
  ///
end;

function TControlFormRemoteShell.GetFrameByInstanceId(const AInstanceId : TGUID) : TFrameRemoteShellInstance;
begin
  result := nil;
  ///

  for var I := 0 to Pages.PageCount -1 do begin
    var ATab := Pages.Pages[I];
    ///

    if (ATab.Controls[0] is TFrameRemoteShellInstance) and
       (TFrameRemoteShellInstance(ATab.Controls[0]).InstanceId = AInstanceId) then begin

       result := TFrameRemoteShellInstance(ATab.Controls[0]);

       ///
       break;
    end;
  end;
end;

function TControlFormRemoteShell.GetFrameByTab(const ATab : TTabSheet) : TFrameRemoteShellInstance;
begin
  result := nil;
  if not Assigned(ATab) then
    Exit();

  if (ATab.Controls[0] is TFrameRemoteShellInstance) then
    result := TFrameRemoteShellInstance(ATab.Controls[0]);
end;

procedure TControlFormRemoteShell.NewShellInstance1Execute(Sender: TObject);
begin
  ButtonNewInstanceClick(ButtonNewInstance);
end;

procedure TControlFormRemoteShell.ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject);
begin
  inherited;
  ///

  var AOptixPacket : TOptixPacket := nil;
  try
    // -----------------------------------------------------------------------------------------------------------------
    if AClassName = TOptixShellOutput.ClassName then begin
      AOptixPacket := TOptixShellOutput.Create(ASerializedPacket);

      var AFrame := GetFrameByInstanceId(TOptixShellOutput(AOptixPacket).InstanceId);
      if not Assigned(AFrame) then
        AFrame := StartShellInstance(TOptixShellOutput(AOptixPacket).InstanceId);
      ///

      AFrame.AddOutput(TOptixShellOutput(AOptixPacket).Output);
    end
    // -----------------------------------------------------------------------------------------------------------------
    else if AClassName = TOptixTerminateShellInstance.ClassName then begin
      AOptixPacket := TOptixTerminateShellInstance.Create(ASerializedPacket);

      CloseShellInstance(TOptixTerminateShellInstance(AOptixPacket).InstanceId);
    end;
    // -----------------------------------------------------------------------------------------------------------------
  finally
    if Assigned(AOptixPacket) then
      FreeAndNil(AOptixPacket);
  end;
end;

procedure TControlFormRemoteShell.RenameTab1Click(Sender: TObject);
begin
  if not Assigned(FHitTab) then
    Exit();

  var ATabName : String := FHitTab.Caption;

  if InputQuery('Rename Tab', 'Enter new name', ATabName) then
    FHitTab.Caption := ATabName;
end;

procedure TControlFormRemoteShell.PagesChange(Sender: TObject);
begin
  RefreshActionsButtons();
end;

procedure TControlFormRemoteShell.PagesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  FHitTab := nil;
  ///

  var AIndex := TPageControl(Sender).IndexOfTabAt(MousePos.X, MousePos.y);
  if (AIndex >= 0) and (AIndex <= (TPageControl(Sender).PageCount -1)) then begin
    var ATab := TPageControl(Sender).Pages[AIndex];
    if Assigned(ATab) then begin
      var ATabBound := TPageControl(Sender).TabRect(AIndex);

      var APoint := TPageControl(Sender).ClientToScreen(Point(ATabBound.Left, ATabBound.Bottom));

      FHitTab := ATab;

      PopupTabs.Popup(APoint.X, APoint.Y);
    end;
  end;
end;

procedure TControlFormRemoteShell.PopupTabsPopup(Sender: TObject);
begin
  TOptixVCLHelper.HideAllPopupMenuRootItems(TPopupMenu(Sender));

  if not Assigned(FHitTab) then
    Exit();

//  var AFrame := GetFrameByTab(FHitTab);
//  if not Assigned(AFrame) then
//    Exit();

  if FHitTab.ImageIndex = -1 then
    erminateInstance1.Visible := True;

  CloseTabTerminate1.Visible := True;
  RenameTab1.Visible := True;
end;

procedure TControlFormRemoteShell.RequestNewShellInstance();
begin
  SendCommand(TOptixStartShellInstance.Create());
end;

function TControlFormRemoteShell.TabNameExists(const AName : String) : Boolean;
begin
  result := False;
  ///

  for var N := 0 to Pages.PageCount -1 do begin
    var ATab := Pages.Pages[N];
    ///

    if String.Compare(ATab.Caption, AName, True) = 0 then begin
      result := True;

      break;
    end;
  end;
end;

function TControlFormRemoteShell.StartShellInstance(const AInstanceId : TGUID) : TFrameRemoteShellInstance;

  function GenerateRandomTabName() : String;
  begin
    var I := 0;
    while True do begin
      Inc(I);
      ///

      var ACandidate := Format('Session #%d', [I]);
      if not TabNameExists(ACandidate) then
        Exit(ACandidate);
    end;
  end;

begin
  var ATab := TTabSheet.Create(Pages);

  ATab.PageControl := Pages;
  ATab.Caption     := GenerateRandomTabName();
  ATab.ImageIndex  := -1;

  var AFrame := TFrameRemoteShellInstance.Create(ATab, self, AInstanceId);
  AFrame.Parent := ATab;

  result := AFrame;

  Pages.ActivePage := ATab;

  AFrame.Command.SetFocus();

  // Hacky method to fix annoying issue with Delphi HDPI designing...
  AFrame.Shell.Font.Size   := 9;
  AFrame.Command.Font.Size := 9;

  ///
  RefreshActionsButtons();
end;

procedure TControlFormRemoteShell.BreakActiveShellInstance1Execute(Sender: TObject);
begin
  ButtonBreakClick(ButtonBreak);
end;

procedure TControlFormRemoteShell.ButtonBreakClick(Sender: TObject);
begin
  var ATab := Pages.ActivePage;

  if ATab.ImageIndex <> -1 then
    Exit();

  var AFrame := GetFrameByTab(ATab);
  if not Assigned(AFrame) then
    Exit();

  SendCommand(TOptixBreakShellInstance.Create(AFrame.InstanceId));
end;

procedure TControlFormRemoteShell.ButtonNewInstanceClick(Sender: TObject);
begin
  RequestNewShellInstance();
end;

procedure TControlFormRemoteShell.CloseShellInstance(const AInstanceId : TGUID);
begin
  var AFrame := GetFrameByInstanceId(AInstanceId);
  if not Assigned(AFrame) or not Assigned(AFrame.Owner) or not (AFrame.Owner is TTabSheet) then
    Exit();
  ///

  TTabSheet(AFrame.Owner).ImageIndex := IMAGE_SHELL_CLOSED;

  AFrame.Close();

  ///
  RefreshActionsButtons();
end;

end.
