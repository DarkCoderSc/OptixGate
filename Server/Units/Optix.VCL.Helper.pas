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

uses VCL.Menus, VirtualTrees, VirtualTrees.Types;

type
  TOptixVCLHelper = class
    public
      { TPopupMenu }
      class procedure UpdatePopupMenuRootItemsVisibility(const APopupMenu : TPopupMenu; const AVisible : Boolean); static;
      class procedure HideAllPopupMenuRootItems(const APopupMenu : TPopupMenu); static;
      class procedure ShowAllPopupMenuRootItems(const APopupMenu : TPopupMenu); static;
      class procedure ShowForm(const AForm : TForm); static; // Use Winja code to do that propertly
                                                             // Use ShowForm (custom) everytime we need to show a control form
                                                             // Implement show form in Control Forms list
                                                             // Also implement in Control Forms Row the extended information (process = number of enumerated process)
  end;

  TOptixVirtualTreesHelper = class
    public
      class function GetVisibleNodesCount(const AVST : TVirtualStringTree) : UInt64; static;
  end;

implementation

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

end.
