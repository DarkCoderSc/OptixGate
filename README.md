# 👽 Optix Gate

![Optix Gate](Assets/featured_image.png)

Optix is a secure, open-source (GPLv3) multi-purpose Remote Access Tool for Windows. It operates using reverse connections and supports OpenSSL for securing communications between the server and its clients through mutual authentication (mTLS).

> Optix name is a reference to an old application from the early 2000s, and the program's logo is also a direct reference to it. The connection is a tribute, which is a recurring theme in many of my projects.

## 💡 Optix can be used for:

- Securely manage your Windows machine fleet (Note: for production use, make sure to run only the version with OpenSSL support if your goal is remote administration of your infrastructure).
- Speed up solving CTF challenges that focus on the Windows ecosystem.
- Certain pentesting engagements, for the same reasons as with CTFs.

However, when it comes to Red Team missions, note that this tool is of no use. The simple reason is that it is not designed for stealth. A Remote Access Tool of this nature is inherently noisy, does not operate through covert channels (by design), and would very likely lead to quick detection and failure in such contexts.

The project is developed in Delphi. In recent years, Embarcadero has made a significant move by offering a completely [free Community Edition of Delphi](https://www.embarcadero.com/products/delphi/starter/free-download) for students and open-source projects. This edition provides access to the latest version of the IDE at no cost, as long as you comply with their licensing terms.

*While Optix Gate is built using the latest professional version of Delphi (currently 13), it has been carefully designed to work seamlessly with the latest version of the free Community Edition.*

## ⚙️ Key Features

![Key Features](Assets/key_features.png)

## 📘 What will you learn?

![What will you learn](Assets/what_will_you_learn.png)

And additional related skills

## 📚 Project Wiki

<div align="center">
<br/>
<a href="https://github.com/DarkCoderSc/OptixGate/wiki">
    <img src="Assets/wiki_button.png" width="200"/>
</a>
<p>
    For more information about its engine, protocol, usage and available features
</p>
</div>

## ⚠️ Disclaimer

This software and code is provided for educational purposes and general informational use only. While it may be applicable in real-world scenarios and can be used in legally compliant contexts, the software and code is provided "as is" without any warranties, express or implied, including but not limited to warranties of merchantability, fitness for a particular purpose, or non-infringement.

The developer makes no representations or guarantees regarding the accuracy, reliability, suitability, or effectiveness of the software when used in any specific application or environment, including but not limited to professional, commercial, or legal contexts.

By using this software and code, you acknowledge and agree that:

- You are solely responsible for how the software is used and for ensuring its suitability for your specific needs.
- You assume all risks associated with its use, including any direct, indirect, incidental, or consequential damages that may result.
- The developer shall not be held liable for any loss, damage, or legal claims arising from your use or misuse of the software, whether in an educational, personal, or professional capacity.

Modifying or redistributing the code is done entirely at your own responsibility.

Use of this software and code constitutes acceptance of this disclaimer. If you do not agree to these terms, you should not use the software and code.

> The GPLv3 license allows anyone to use, modify, and distribute the software freely, as long as any derivative works are also shared under the same license. It requires that **source code be made available when distributing binaries**, provides no warranty or liability for the author, and includes protections against patent claims and hardware restrictions that prevent user modifications. Commercial use is permitted, provided all GPLv3 obligations are met.

---

⚠️ **IMPORTANT** : *Optix will not, and will never, implement code that facilitates malicious usage. This includes features intended solely for malicious purposes, evasion or stealth techniques, client configuration patching, and persistence mechanisms. If you are conducting a penetration test and require such features, it is your responsibility to implement them and compile your own version.*

## 💘 Special thanks goes to

![Sponsored by Embarcadero](Assets/sponsored_embarcadero.png)

Optix Gate is sponsored by Embarcadero, the company behind Delphi, C++ Builder, and many other groundbreaking developer tools. Their invaluable support makes this open-source project possible, and I am deeply grateful for their commitment.

---

I would like to also thank the following people for their support, extensive testing, and early feedback

[Embarcadero](https://www.embarcadero.com), [Mr.NOODLE](https://x.com/bragames2), [Euz](https://x.com/_Euzebius), [Mudpak](https://x.com/_mudpak), [Tristan Manzano](https://www.linkedin.com/in/tristan%2Dmanzano%2D963223103/), [Laurent Minne](https://www.linkedin.com/in/laurent%2Dminne/)

---

*Some icons, either free or purchased, may be used in project documentation, the project wiki, and the project itself, in accordance with their respective license requirements. The creators and publishers of these icons are acknowledged as follows: [Icons8](https://icons8.com/) and [Axialis](http://axialis.com). All rights are reserved to their respective owners.*