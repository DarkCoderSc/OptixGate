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

unit Optix.FileSystem.Helper;

interface

uses System.Classes;

type
  TDriveType = (
    dtUnknown,
    dtNoRootDir,
    dtRemovable,
    dtFixed,
    dtRemote,
    dtCDROM,
    dtRAMDisk
  );

  TFileSystemHelper = class
  public
    class function GetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String; var ADriveType : TDriveType) : Boolean; static;
    class function TryGetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String; var ADriveType : TDriveType) : Boolean;
  end;

  function DriveTypeToString(const AValue : TDriveType) : String;

implementation

uses Winapi.Windows, System.Sysutils;

{ _.ElevatedStatusToString }
function DriveTypeToString(const AValue : TDriveType) : String;
begin
  result := 'Unknown';
  ///

  case AValue of
    dtUnknown   : result := 'Unknown';
    dtNoRootDir : result := 'No Root Dir';
    dtRemovable : result := 'Removable';
    dtFixed     : result := 'Fixed';
    dtRemote    : result := 'Network';
    dtCDROM     : result := 'CD-ROM';
    dtRAMDisk   : result := 'RAM Disk';
  end;
end;

{ TFileSystemHelper.GetDriveInformation }
class function TFileSystemHelper.GetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String; var ADriveType : TDriveType) : Boolean;
begin
  var AOldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    ADriveLetter := IncludeTrailingPathDelimiter(ExtractFileDrive(ADriveLetter));
    ///

    var ADummy        : DWORD;
    var ABufferName   : array[0..MAX_PATH-1] of WideChar;
    var ABufferFormat : array[0..MAX_PATH-1] of WideChar;

    FillChar(ABufferName, MAX_PATH, #0);
    FillChar(ABufferFormat, MAX_PATH, #0);

    result := GetVolumeInformation(
                                    PWideChar(ADriveLetter),
                                    ABufferName,
                                    MAX_PATH,
                                    nil,
                                    ADummy,
                                    ADummy,
                                    ABufferFormat,
                                    MAX_PATH
    );

    {
      Conv to String
    }
    AName   := String(ABufferName);
    AFormat := String(ABufferFormat);

    {
      Get Drive Type
    }
    case GetDriveType(PWideChar(ADriveLetter)) of
      1 : ADriveType := dtNoRootDir; // DRIVE_NO_ROOT_DIR
      2 : ADriveType := dtRemovable; // DRIVE_REMOVABLE
      3 : ADriveType := dtFixed;     // DRIVE_FIXED
      4 : ADriveType := dtRemote;    // DRIVE_REMOTE
      5 : ADriveType := dtCDROM;     // DRIVE_CDROM
      6 : ADriveType := dtRAMDisk;   // DRIVE_RAMDISK
      else
        ADriveType := dtUnknown;
    end;
  finally
    SetErrorMode(AOldErrorMode);
  end;
end;

{ TFileSystemHelper.TryGetDriveInformation }
class function TFileSystemHelper.TryGetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String; var ADriveType : TDriveType) : Boolean;
begin
  AName      := '';
  AFormat    := '';
  ADriveType := dtUnknown;
  ///
  try
    result := GetDriveInformation(ADriveLetter, AName, AFormat, ADriveType);
  except
    result := False;
  end;
end;

end.
