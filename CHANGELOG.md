# Changelogs

## 2.0.1 (Feb 2026)

Version 2.0.1 is a significant quality update. The primary focus is to further enhance its already outstanding stability and simplify the build process for users with limited experience.

- Several code segments have been optimized and simplified, and minor memory leaks have been addressed.
- The external library **XSuperObject** has been fully replaced by the native, modern Delphi alternative, **System.JSON**.
- The **OMultiPanel** external component has been integrated into the NeoFlat package for greater convenience, reducing the installation steps for users with limited Delphi experience.
- The `Dist.bat` script has been enhanced for users with a **Delphi Professional license**, allowing them to build all project components and modes across all architectures without needing to open the IDE.

## 2.0.0 (Feb 2026)

- Major codebase restructuring and refactoring. The architecture is now more logical and modular, making maintenance easier and allowing faster access to topic-specific functions.
- General code optimizations, multiple bug fixes, and overall performance improvements.
- Updated OpenSSL to version 1.1w (planned upgrade to the latest stable release in a future update).
- Introduced Neo Flat v1.0, a fully hand-crafted component suite for Delphi, designed to faithfully reproduce the visual style of many Delphi applications from the 2000s era. Inspired by Fast Style Component, Neo Flat is a complete remake and enhancement, rebuilt from the ground up to ensure compatibility and optimal performance on modern Windows versions. This release establishes the definitive visual direction of the application, with further refinements and feature expansions planned for future updates.
- Integrated FamFamFam (HDPI) icons to further reinforce the early-2000s desktop application design.

## 1.4.0 (Dec 2025)

#### Registry Manager
- Create new keys.
- Delete existing keys.
- Rename keys.
- Create new values (String, Multi-Line String, Binary, DWORD, QWORD) with the ability to switch between modes (Convert data when applicable).
- Edit values.
- Delete values.
- Rename values.
- Fix issues in the registry tree display and navigation.

#### File Manager
- Fix issues in the folder tree display and navigation.

#### File Content Streaming
- Redesigned interface for improved user experience.
- Added new hex table component for easier viewing and editing.

#### Miscellaneous
- Code refactored and improved for clarity, maintainability and extensibility - further work is expected progressively.
- Updated Optix command serializer to support `TObjectList<T>` and `TList<T>`.
- Custom hex table component added with support for read-only mode, editing, and dynamic memory expansion.
- Removed non-GUI client from repository. Only the Client GUI remains.

## 1.3.0 (Oct 2025)

The Content Reader feature introduces a new capability that allows users to remotely open and view readable files in a dedicated editor. Currently available for files only, this feature enables instant server-side access to any file, regardless of its size.

To optimize performance, the remote file is paginated, meaning that only a user-defined portion (chunk) of the file is transmitted and displayed at a time, rather than transferring the entire file. The content is presented as a hexadecimal table, with built-in tools to extract and highlight ANSI and Unicode strings from the streamed data.

This approach ensures that even very large files can be opened instantly for view-only access, without requiring a full download or local storage.

This feature has been integrated into the file manager for readable files. You can also manually stream a remote file through a dedicated dialog. 

Finally, this release includes several minor changes and improvements.

## 1.2.0 (Oct 2025)

* Registry Manager has been introduced, currently available for browsing only. Users can browse registry hives and keys and view values (e.g., DWORD, QWORD, String, Binary, etc.). *Creation, deletion, and modification of keys or values will be implemented in a future update.*
* A bug in the File Manager Folder Tree related to the 'Go To Path' function has been fixed. The complete destination path, including parent and child hierarchy, is now correctly built with proper permission resolution. This fix required a significant structural change.
* File Manager now supports relative paths. Using `..` will resolve correctly, and the path format has been standardized to prevent potential errors.
* Various other minor improvements and optimizations have been implemented.

ⓘ The Registry Manager and File Manager Folder Tree showcase another valuable concept: generic programming using the Delphi programming language.

## 1.1.0 (Sept 2025)

* **Folder Tree View Added to File Manager:** The browsed folder tree is now optionally available in the file manager (default: enabled).
* **File Upload Feedback Added:** When a file is uploaded, all file manager windows are notified if the new file matches the current directory (reactive update).
* **Significant Protocol Improvements:** While not directly visible in the compiled version, the underlying code has been greatly enhanced. Optix command dispatching is now handled generically, and command/response serialization and deserialization are partially automated using the power of Delphi RTTI for most basic object and property types. This considerably reduces the time required to implement new commands and responses, minimizes potential implementation errors, and makes the project much easier to expand.

## 1.0.0 (Sept 2025)

* **Column sorting:** Improved data navigation with sortable columns.
* **ZLib data compression:** Optimized performance through compression of Optix packets (JSON commands and responses).
* **File manager enhancements:** Added backward/forward navigation for easier file browsing, and go to location.
* **Execute-only folders:** Introduced a dedicated folder icon to clearly identify execute-only directories.
* **Multi-listener support:** Manage multiple servers with saved configurations and automatic startup.
* **IPv6 compatibility:** Full support for modern IPv6 networking alongside IPv4.
* **Code improvements:** General optimizations and refinements for better stability and maintainability.

## 1.0.0 Alpha 1 (August 2025)

* First release ever.