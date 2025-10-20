# Optix Gate

![Optix Gate](Assets/featured_image.png)

Optix is a secure, open-source (GPLv3) multi-purpose Remote Access Tool for Windows. It operates using reverse connections and supports OpenSSL for securing communications between the server and its clients through mutual authentication (mTLS).

Optix can be used for:

- Securely manage your Windows machine fleet (Note: for production use, make sure to run only the version with OpenSSL support if your goal is remote administration of your infrastructure).
- Speed up solving CTF challenges that focus on the Windows ecosystem.
- Certain pentesting engagements, for the same reasons as with CTFs.

However, when it comes to Red Team missions, note that this tool is of no use. The simple reason is that it is not designed for stealth. A Remote Access Tool of this nature is inherently noisy, does not operate through covert channels (by design), and would very likely lead to quick detection and failure in such contexts.

Furthermore, studying the source code will allow you to gain a deeper understanding of this type of program, which is highly comprehensive and covers multiple domains, including, but not limited to: network programming, system programming, cryptography, Windows API implementation and Windows internals, graphical interface creation and management, as well as parallelism and multithreading. This project complements the resources available on the Malware Gallery platform ([Malware Gallery](https://www.malwaregallery.com)), and the two will mutually enhance each other.

It is important to distinguish between a Remote Access Tool and a related category of malware known as a Remote Access Trojan (RAT). The key difference lies in intent: a Trojan is any program that disguises itself as legitimate, for example, in appearance or execution pretext, but actually performs malicious actions without the knowledge of the user.

⚠️ *Optix will not, and will never, implement code that facilitates such usage. This includes features intended solely for malicious purposes, evasion or stealth techniques, client configuration patching, and persistence mechanisms. If you are conducting a penetration test and require such features, it is your responsibility to implement them and compile your own version.*

Finally, Optix is a reference to an old Remote Access Trojan of the same name from the early 2000s, and the program's logo is also a direct reference to it. The connection is a tribute, which is a recurring theme in many of my projects.

---

![Sponsored by Embarcadero](Assets/sponsored_embarcadero.png)

Optix Gate is sponsored by Embarcadero, the company behind Delphi, C++ Builder, and many other groundbreaking developer tools. Their invaluable support makes this open-source project possible, and I am deeply grateful for their commitment.

The project is developed in Delphi. In recent years, Embarcadero has made a significant move by offering a completely [free Community Edition of Delphi](https://www.embarcadero.com/products/delphi/starter/free-download) for students and open-source projects. This edition provides access to the latest version of the IDE at no cost, as long as you comply with their licensing terms.

*While Optix Gate is built using the latest professional version of Delphi (currently 13), it has been carefully designed to work seamlessly with the free Community Edition.*

---

## Key Features

* Multi-threaded and parallelized environment for managing multiple clients simultaneously
* Mutual authentication (**mTLS**) between peers using **TLS 1.3** with **AES-256-GCM SHA-384** via OpenSSL for secure communication
* Multi-listener (server) support with compatibility for both **IPv4** and modern **IPv6**
* Remote shell with multiplexing
* Remote file manager
* Remote process manager
* Remote registry manager
* Remote Content Streaming (File)
* 100% free, 100% open-source, forever

…and much more to do, much more to come.

---

⚠️ For more information about its engine, protocol, usage, and available features: [please refer to the wiki](https://github.com/DarkCoderSc/OptixGate/wiki)

---

### What will you learn?

* Windows APIs and system internals
* Native socket programming
* Implementing OpenSSL on top of native socket programming
* Designing, writing, and maintaining custom network protocols
* Advanced concepts in the Delphi programming language

And additional related skills

### The future?

Many additional actions are planned for existing features, and several new capabilities are also in development. One of my priorities is to enable the execution of .NET assemblies and PE files directly from memory. In addition, I plan to implement a Func-In engine to make the application more extensible, allowing seamless integration of third-party tools instead of reinventing functionality that already exists and performs well.

Stability is my top priority; new features come second.

## Changelogs

### 1.3.0 (Oct 2025)

The Content Reader feature introduces a new capability that allows users to remotely open and view readable files in a dedicated editor. Currently available for files only, this feature enables instant server-side access to any file, regardless of its size.

To optimize performance, the remote file is paginated, meaning that only a user-defined portion (chunk) of the file is transmitted and displayed at a time, rather than transferring the entire file. The content is presented as a hexadecimal table, with built-in tools to extract and highlight ANSI and Unicode strings from the streamed data.

This approach ensures that even very large files can be opened instantly for view-only access, without requiring a full download or local storage.

This feature has been integrated into the file manager for readable files. You can also manually stream a remote file through a dedicated dialog. 

Finally, this release includes several minor changes and improvements.

### 1.2.0 (Oct 2025)

* Registry Manager has been introduced, currently available for browsing only. Users can browse registry hives and keys and view values (e.g., DWORD, QWORD, String, Binary, etc.). *Creation, deletion, and modification of keys or values will be implemented in a future update.*
* A bug in the File Manager Folder Tree related to the 'Go To Path' function has been fixed. The complete destination path, including parent and child hierarchy, is now correctly built with proper permission resolution. This fix required a significant structural change.
* File Manager now supports relative paths. Using `..` will resolve correctly, and the path format has been standardized to prevent potential errors.
* Various other minor improvements and optimizations have been implemented.

ⓘ The Registry Manager and File Manager Folder Tree showcase another valuable concept: generic programming using the Delphi programming language.

### 1.1.0 (Sept 2025)

* **Folder Tree View Added to File Manager:** The browsed folder tree is now optionally available in the file manager (default: enabled).
* **File Upload Feedback Added:** When a file is uploaded, all file manager windows are notified if the new file matches the current directory (reactive update).
* **Significant Protocol Improvements:** While not directly visible in the compiled version, the underlying code has been greatly enhanced. Optix command dispatching is now handled generically, and command/response serialization and deserialization are partially automated using the power of Delphi RTTI for most basic object and property types. This considerably reduces the time required to implement new commands and responses, minimizes potential implementation errors, and makes the project much easier to expand.

### 1.0.0 (Sept 2025)

* **Column sorting:** Improved data navigation with sortable columns.
* **ZLib data compression:** Optimized performance through compression of Optix packets (JSON commands and responses).
* **File manager enhancements:** Added backward/forward navigation for easier file browsing, and go to location.
* **Execute-only folders:** Introduced a dedicated folder icon to clearly identify execute-only directories.
* **Multi-listener support:** Manage multiple servers with saved configurations and automatic startup.
* **IPv6 compatibility:** Full support for modern IPv6 networking alongside IPv4.
* **Code improvements:** General optimizations and refinements for better stability and maintainability.

### 1.0.0 Alpha 1 (August 2025)

* First release ever.

## Dependencies

OptixGate (Server) and the Client GUI both require the [Virtual Treeview component from JAM Software](https://www.jam-software.com/virtual-treeview); it is recommended to use the latest available version. OptixGate additionally requires OMultiPanel to be compiled, which is necessary for Remote Shell panel splitting. [XSuperObject](https://github.com/onryldz/x-superobject) library from vkrapotkin and [OMultiPanel](https://sourceforge.net/projects/omultipanel/) from Ondřej Pokorný are also used.  

*You will find the third-party Delphi libraries used in the project source code, matching the exact versions I worked with, in the folder `Libraries > Delphi`.*

It is recommended to use the latest version of Delphi. You can install the [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter/free-download), which is available free of charge and fully sufficient to compile the entire project.

For OpenSSL support, it is recommended to use the distributed DLLs provided in the Library folder. You may use your own compiled version at your own risk. The OpenSSL version should be at least equal to or higher than the version included in the project, though compatibility is not guaranteed.

Instructions for installing Delphi components and compiling project are not provided; however, resources are readily available online for those with basic familiarity with Delphi.

## Disclaimer

This software and code is provided for educational purposes and general informational use only. While it may be applicable in real-world scenarios and can be used in legally compliant contexts, the software and code is provided "as is" without any warranties, express or implied, including but not limited to warranties of merchantability, fitness for a particular purpose, or non-infringement.

The developer makes no representations or guarantees regarding the accuracy, reliability, suitability, or effectiveness of the software when used in any specific application or environment, including but not limited to professional, commercial, or legal contexts.

By using this software and code, you acknowledge and agree that:

- You are solely responsible for how the software is used and for ensuring its suitability for your specific needs.
- You assume all risks associated with its use, including any direct, indirect, incidental, or consequential damages that may result.
- The developer shall not be held liable for any loss, damage, or legal claims arising from your use or misuse of the software, whether in an educational, personal, or professional capacity.

Modifying or redistributing the code is done entirely at your own responsibility.

Use of this software and code constitutes acceptance of this disclaimer. If you do not agree to these terms, you should not use the software and code.

> The GPLv3 license allows anyone to use, modify, and distribute the software freely, as long as any derivative works are also shared under the same license. It requires that **source code be made available when distributing binaries**, provides no warranty or liability for the author, and includes protections against patent claims and hardware restrictions that prevent user modifications. Commercial use is permitted, provided all GPLv3 obligations are met.

## Contact

I will not respond to requests regarding custom demands, specific versions of the program, or provide support on installation, configuration, or usage. This program is intended for users with a minimum level of technical knowledge.

Feedback, however, is highly appreciated. You may contact me via email at jplesueur[at]phrozen.io, using the following PGP key:

```
-----BEGIN PGP PUBLIC KEY BLOCK-----
xjMEZQDTERYJKwYBBAHaRw8BAQdAUICkUuZmCktHcxi1tfHGQTTT/TrJCOqA
jymKVkrN6q/NK2pwbGVzdWV1ckBwaHJvemVuLmlvIDxqcGxlc3VldXJAcGhy
b3plbi5pbz7CjAQQFgoAPgWCZQDTEQQLCQcICZAqv1dYE4gcUwMVCAoEFgAC
AQIZAQKbAwIeARYhBHEqqY9tzg5HN9EB+yq/V1gTiBxTAACuFQD/UllBJFEY
CqE4+W/aR/vX+TLJmvNcmfTCcLE4DLNihDsA/2rms3XBBySjSmGTBizIvAYu
aB8fEEjtjUNGU6VOGL4IzjgEZQDTERIKKwYBBAGXVQEFAQEHQIn/iNRs3IO1
Eh/8FG6+1FV/nEywsK28hDgXwOzDQIJxAwEIB8J4BBgWCAAqBYJlANMRCZAq
v1dYE4gcUwKbDBYhBHEqqY9tzg5HN9EB+yq/V1gTiBxTAAD7sAD/fkYNudvg
fEiBdAPaKVXCzTQP2tT8uBAGVLPjzAmXzHEBAKlQ5auOh+8tyfeK73RfCbW7
zRt12rJmulmCvpFscq8G
=3Qtg
-----END PGP PUBLIC KEY BLOCK-----
```

## Special Thanks

I would like to thank the following people for their support, extensive testing, and early feedback

- [Embarcadero](https://www.embarcadero.com)
- [Mr.NOODLE](https://x.com/bragames2)
- [Euz](https://x.com/_Euzebius)
- [Mudpak](https://x.com/_mudpak)
- [Tristan Manzano](https://www.linkedin.com/in/tristan%2Dmanzano%2D963223103/)
- [Laurent Minne](https://www.linkedin.com/in/laurent%2Dminne/)
