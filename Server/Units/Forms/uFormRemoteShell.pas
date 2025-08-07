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

unit uFormRemoteShell;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, __uBaseFormControl__, Vcl.ComCtrls, uFrameRemoteShellInstance, Vcl.Menus,
  XSuperObject;

type
  TFormRemoteShell = class(TBaseFormControl)
    Pages: TPageControl;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    NewInstance1: TMenuItem;
    StatusBar: TStatusBar;
    PopupTabs: TPopupMenu;
    erminateInstance1: TMenuItem;
    N1: TMenuItem;
    CloseTabTerminate1: TMenuItem;
    RenameTab1: TMenuItem;
    procedure NewInstance1Click(Sender: TObject);
    procedure PagesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  private
    {@M}
    procedure RequestNewShellInstance();
    function StartShellInstance(const AInstanceId : TGUID) : TFrameRemoteShellInstance;
    procedure CloseShellInstance(const AInstanceId : TGUID);
    function GetFrameByInstanceId(const AInstanceId : TGUID) : TFrameRemoteShellInstance;
  public
    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); override;
  end;

var
  FormRemoteShell: TFormRemoteShell;

implementation

uses uFormMain, Optix.Func.Commands, Optix.Protocol.Packet, Optix.Func.Shell, Optix.Constants;

{$R *.dfm}

function TFormRemoteShell.GetFrameByInstanceId(const AInstanceId : TGUID) : TFrameRemoteShellInstance;
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

procedure TFormRemoteShell.ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject);
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

procedure TFormRemoteShell.NewInstance1Click(Sender: TObject);
begin
  RequestNewShellInstance();
end;

procedure TFormRemoteShell.PagesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  var AIndex := TPageControl(Sender).IndexOfTabAt(MousePos.X, MousePos.y);
  if (AIndex >= 0) and (AIndex <= (TPageControl(Sender).PageCount -1)) then begin
    var ATab := TPageControl(Sender).Pages[AIndex];
    if Assigned(ATab) then begin
      var ATabBound := TPageControl(Sender).TabRect(AIndex);

      var APoint := TPageControl(Sender).ClientToScreen(Point(ATabBound.Left, ATabBound.Bottom));

      PopupTabs.Popup(APoint.X, APoint.Y);
    end;
  end;
end;

procedure TFormRemoteShell.RequestNewShellInstance();
begin
  SendCommand(TOptixStartShellInstance.Create());
end;

function TFormRemoteShell.StartShellInstance(const AInstanceId : TGUID) : TFrameRemoteShellInstance;

  function GenerateRandomTabName() : String;
  begin
    var I := 0;
    while True do begin
      Inc(I);
      ///

      var ACandidate := Format('Session #%d', [I]);

      for var N := 0 to Pages.PageCount -1 do begin
        var ATab := Pages.Pages[N];
        ///

        if String.Compare(ATab.Caption, ACandidate, True) = 0 then
          break
        else
          Exit(ACandidate);
      end;
    end;
  end;

begin
  var ATab := TTabSheet.Create(Pages);

  ATab.PageControl := Pages;
  ATab.Caption     := GenerateRandomTabName();
  ATab.ImageIndex  := IMAGE_SHELL_RUNNING;

  var AFrame := TFrameRemoteShellInstance.Create(ATab, self, AInstanceId);
  AFrame.Parent := ATab;

  result := AFrame;

  Pages.ActivePage := ATab;

  AFrame.Command.SetFocus();

  // Hacky method to fix annoying issue with Delphi HDPI designing...
  AFrame.Shell.Font.Size   := 9;
  AFrame.Command.Font.Size := 9;
end;

procedure TFormRemoteShell.CloseShellInstance(const AInstanceId : TGUID);
begin
  var AFrame := GetFrameByInstanceId(AInstanceId);
  if not Assigned(AFrame) and Assigned(AFrame.Owner) and (AFrame.Owner is TTabSheet) then
    Exit();
  ///

  TTabSheet(AFrame.Owner).ImageIndex := IMAGE_SHELL_CLOSED;

  AFrame.Close();
end;

- Faire le break (CTRL+C)
- Coder les actions (Close Tab, Terminate (si pas deja), Rename Tab)
- Finir le design du remote shell

end.
