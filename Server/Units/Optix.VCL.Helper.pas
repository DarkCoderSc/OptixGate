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

unit Optix.VCL.Helper;

interface

uses VCL.Menus, VirtualTrees, VirtualTrees.Types, VCL.Forms;

type
  TOptixVCLHelper = class
    public
      { TPopupMenu }
      class procedure UpdatePopupMenuRootItemsVisibility(const APopupMenu : TPopupMenu; const AVisible : Boolean); static;
      class procedure HideAllPopupMenuRootItems(const APopupMenu : TPopupMenu); static;
      class procedure ShowAllPopupMenuRootItems(const APopupMenu : TPopupMenu); static;
      class procedure ShowForm(const AForm : TForm); static;
  end;

  TOptixVirtualTreesHelper = class
    public
      class function GetVisibleNodesCount(const AVST : TVirtualStringTree) : UInt64; static;
      class function GetColumnIndexByName(const AVST : TVirtualStringTree; const AName : String) : Integer; static;
      class procedure UpdateColumnVisibility(const AVST : TVirtualStringTree; const AName : String; AVisible : Boolean); static;
  end;

implementation

uses Winapi.Windows, Winapi.Messages, System.SysUtils;

(* TOptixVirtualTreesHelper *)

{ TOptixVirtualTreesHelper.GetVisibleNodesCount }
class function TOptixVirtualTreesHelper.GetVisibleNodesCount(const AVST : TVirtualStringTree) : UInt64;
begin
  result := 0;
  ///

  if not Assigned(AVST) then
    Exit();

  for var pNode in AVST.Nodes do begin
    if vsVisible in pNode.States then
      Inc(result);
  end;
end;

{ TOptixVirtualTreesHelper.GetColumnIndexByName }
class function TOptixVirtualTreesHelper.GetColumnIndexByName(const AVST : TVirtualStringTree; const AName : String) : Integer;
begin
  result := -1;
  ///

  if not Assigned(AVST) then
    Exit();

  for var AIndex := 0 to AVST.Header.Columns.Count -1 do begin
    if String.Compare(AName, AVST.Header.Columns.Items[AIndex].Text, True) = 0 then begin
      result := AIndex;

      ///
      break;
    end;
  end;
end;

{ TOptixVirtualTreesHelper.UpdateColumnVisibility }
class procedure TOptixVirtualTreesHelper.UpdateColumnVisibility(const AVST : TVirtualStringTree; const AName : String; AVisible : Boolean);
begin
  var AColumnIndex := GetColumnIndexByName(AVST, AName);
  if AColumnIndex = -1 then
    Exit();
  ///

  var AColumn := AVST.Header.Columns.Items[AColumnIndex];

  if AVisible then
    AColumn.Options := AColumn.Options + [coVisible]
  else
    AColumn.Options := AColumn.Options - [coVisible];
end;

(* TOptixVCLHelper *)

{ TOptixVCLHelper.UpdatePopupMenuRootItemsVisibility }
class procedure TOptixVCLHelper.UpdatePopupMenuRootItemsVisibility(const APopupMenu : TPopupMenu; const AVisible : Boolean);
begin
  if not Assigned(APopupMenu) then
    Exit();
  ///

  for var I := 0 to APopupMenu.Items.Count -1 do
    APopupMenu.Items[I].Visible := AVisible;
end;

{ TOptixVCLHelper.HideAllPopupMenuRootItems }
class procedure TOptixVCLHelper.HideAllPopupMenuRootItems(const APopupMenu : TPopupMenu);
begin
  UpdatePopupMenuRootItemsVisibility(APopupMenu, False);
end;

{ TOptixVCLHelper.ShowAllPopupMenuRootItems }
class procedure TOptixVCLHelper.ShowAllPopupMenuRootItems(const APopupMenu : TPopupMenu);
begin
  UpdatePopupMenuRootItemsVisibility(APopupMenu, True);
end;

{ TOptixVCLHelper.ShowForm }
class procedure TOptixVCLHelper.ShowForm(const AForm : TForm);
begin
  if not Assigned(AForm) then
    Exit();
  ///

  if IsIconic(AForm.Handle) then begin
    if Application.MainForm.Handle = AForm.Handle then
      SendMessage(AForm.Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      ShowWindow(AForm.Handle, SW_RESTORE);
  end else
    AForm.Show();
end;

end.
