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



unit OptixCore.ClassesRegistry;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils, System.Rtti, System.TypInfo,

  Generics.Collections;
// ---------------------------------------------------------------------------------------------------------------------

type
  TClassesRegistry = class
  private
    class var FRegisteredClasses : TDictionary<String, TClass>;
  public
    {@C}
    class constructor Create();
    class destructor Destroy();

    {@M}
    class function CreateInstance(const AClassName: String; const AParams: array of TValue) : TObject; static;
    class procedure RegisterClass(const AClass : TClass); static;
    class procedure RegisterClasses(const AClasses : array of TClass); static;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  OptixCore.Commands.FileSystem, OptixCore.Commands.Process, OptixCore.Commands,
  OptixCore.Commands.Shell, OptixCore.Task.ProcessDump, OptixCore.Commands.Base,
  OptixCore.SessionInformation, OptixCore.LogNotifier, OptixCore.Commands.Registry,
  OptixCore.Commands.ContentReader;
// ---------------------------------------------------------------------------------------------------------------------

class constructor TClassesRegistry.Create();
begin
  FRegisteredClasses := TDictionary<String, TClass>.Create();
end;

class destructor TClassesRegistry.Destroy();
begin
  if Assigned(FRegisteredClasses) then
    FreeAndNil(FRegisteredClasses);
end;

class function TClassesRegistry.CreateInstance(const AClassName: String; const AParams: array of TValue) : TObject;
begin
  result := nil;
  ///

  if not Assigned(FRegisteredClasses) or (FRegisteredClasses.Count = 0) then
    Exit();
  ///

  var AClass : TClass;
  if not FRegisteredClasses.TryGetValue(AClassName, AClass) then
    Exit();
  ///

  var AContext := TRttiContext.Create();

  var AType := AContext.GetType(AClass);

  for var AMethod in AType.GetMethods() do begin
    if not AMethod.IsConstructor then
      continue;
    ///

    var AParameters := AMethod.GetParameters();
    if Length(AParameters) = Length(AParams) then begin
      for var I := Low(AParameters) to High(AParameters) do begin
        if AParams[I].IsType(AParameters[I].ParamType.Handle) then begin
          result := AMethod.Invoke(AClass, AParams).AsObject();

          break;
        end;
      end;

      if Assigned(result) then
        break;
    end;
  end;
end;

class procedure TClassesRegistry.RegisterClass(const AClass : TClass);
begin
  if Assigned(FRegisteredClasses) and not FRegisteredClasses.ContainsKey(AClass.ClassName) then
    FRegisteredClasses.Add(AClass.ClassName, AClass);
end;

class procedure TClassesRegistry.RegisterClasses(const AClasses : array of TClass);
begin
  for var AClass in AClasses do
    RegisterClass(AClass);
end;

initialization
  (* Commands *)
  TClassesRegistry.RegisterClasses([
    // General
    TOptixCommandTerminateCurrentProcess,
    TOptixCommandReceiveSessionInformation,

    // File System Commands
    TOptixCommandReceiveFileInformation,
    TOptixCommandGetUploadedFileInformation,
    TOptixCommandEnumDrives,
    TOptixCommandEnumDirectoryFiles,
    TOptixCommandDownloadFile,
    TOptixCommandUploadFile,

    // System & Process Commands
    TOptixCommandTerminateProcess,
    TOptixCommandDumpProcess,
    TOptixCommandEnumRunningProcesses,

    // Shell Commands
    TOptixCommandCreateShellInstance,
    TOptixCommandDeleteShellInstance,
    TOptixCommandSigIntShellInstance,
    TOptixCommandWriteShellInstance,
    TOptixCommandReadShellInstance,

    // Logs
    TOptixCommandReceiveLogMessage,
    TOptixCommandReceiveTransferException,

    // Registry Manager
    TOptixCommandEnumRegistryHives,
    TOptixCommandEnumRegistryKeys,
    TOptixCommandCreateRegistryKey,
    TOptixCommandDeleteRegistryKey,
    TOptixCommandSetRegistryValue,
    TOptixCommandSetRegistryKeyName,
    TOptixCommandSetRegistryValueName,
    TOptixCommandDeleteRegistryValue,

    // File Readers
    TOptixCommandCreateFileContentReader,
    TOptixCommandDeleteContentReader,
    TOptixCommandGetContentReaderPage,
    TOptixCommandReadContentReaderPageFirstPage,
    TOptixCommandReadContentReaderPage,

    (* Tasks *)

    TOptixTaskResult,
    TOptixTaskCallback,
    TOptixTaskGetProcessDumpResult
  ]);

end.
