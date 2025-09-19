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

unit Optix.Func.Commands.FileSystem;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Winapi.Windows,

  XSuperObject,

  Optix.Func.Commands.Base, Optix.Func.Enum.FileSystem;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixRequestFileInformation = class(TOptixCommandActionResponse)
  private
    FFileName        : String;
    FIsDirectory     : Boolean;

    FFileInformation : TFileInformation;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    procedure DoAction(); override;

    function Serialize() : ISuperObject; override;

    {@C}
    constructor Create(); reintroduce; override;
    constructor Create(const AFileName : String; const AIsDirectory : Boolean); overload;
    destructor Destroy(); override;

    {@G}
    property FileName        : String           read FFileName;
    property IsDirectory     : Boolean          read FIsDirectory;
    property FileInformation : TFileInformation read FFileInformation;
  end;

  {@ALIASES: TOptixRequestFileInformation}
  TOptixRequestUploadedFileInformation = class(TOptixRequestFileInformation);

implementation

(***********************************************************************************************************************

  TOptixRequestFileInformation

(**********************************************************************************************************************)

{ TOptixRequestFileInformation.Create }
constructor TOptixRequestFileInformation.Create();
begin
  inherited Create();
  ///

  FFileInformation := nil;
end;

{ TOptixRequestFileInformation.Create }
constructor TOptixRequestFileInformation.Create(const AFileName : String; const AIsDirectory : Boolean);
begin
  Create();
  ///

  FFileName    := AFileName;
  FIsDirectory := AIsDirectory;
end;

{ TOptixRequestFileInformation.Destroy }
destructor TOptixRequestFileInformation.Destroy();
begin
  if Assigned(FFileInformation) then
    FreeAndNil(FFileInformation);

  ///
  inherited Destroy();
end;

{ TOptixRequestFileInformation.DoAction }
procedure TOptixRequestFileInformation.DoAction();
begin
  inherited;
  ///

  FFileInformation := TFileInformation.Create(FFileName, FIsDirectory);
end;

{ TOptixRequestFileInformation.Serialize }
function TOptixRequestFileInformation.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['FileName']    := FFileName;
  result.B['IsDirectory'] := FIsDirectory;

  if Assigned(FFileInformation) then
    result.O['FileInformation'] := FFileInformation.Serialize();
end;

{ TOptixRequestFileInformation.DeSerialize }
procedure TOptixRequestFileInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FFileName    := ASerializedObject.S['FileName'];
  FIsDirectory := ASerializedObject.B['IsDirectory'];

  if ASerializedObject.Contains('FileInformation') then
    FFileInformation := TFileInformation.Create(ASerializedObject.O['FileInformation']);
end;

end.
