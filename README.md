# HaskellBot
Grow your own Haskeller!
The link to <a href="https://t.me/Haskeller_bot">@Haskeller_bot</a>

![image](https://user-images.githubusercontent.com/84839431/177187973-3f844cbe-1564-4ad3-89b2-a35a6473fe27.png)


Telegram chat-bot for fun. The idea is to grow your own ”haskeller” with the following chat commands: /change_name, /grow, /info, /leaderboard
- `/change_name` - to change the name of your haskeller
- `/grow` - to grow by 1 IQ of your “haskeller” and see the current IQ
- `/info` - to see all the characteristics of your “haskeller” (name, IQ, rank)
- `/leaderboard` - to see the best five "haskellers" (name, IQ)
<p>The initial state of “haskeller”:</p>
    <ul>
  <li>IQ = 0</li>
  <li>name = “Haskeller”</li>
  <li>rank = "Newbie"</li>
    </ul>


# Building
```
git clone https://github.com/Legolass322/HaskellBot
```

#### Installing Stack
Stack is a tool to manage Haskell projects. We used it while development of out product. To install Stack tool:

For most Un*x operating systems, the easiest way to install is to run:

```sh
curl -sSL https://get.haskellstack.org/ | sh
```

or:

```sh
wget -qO- https://get.haskellstack.org/ | sh
```

On Windows, you can download and install <a href=https://get.haskellstack.org/stable/windows-x86_64-installer.exe>the Windows 64-bit Installer</a>.
(this information from <a href=https://docs.haskellstack.org/en/stable/README/>this documentation</a>)
 
#### GHC
GHC is a haskell compiler. It needed to compile your haskell code. Stack can download it:

```sh
stack setup
```

#### Using project via Stack

Building project. It will install all dependencies.
```sh
stack build
```

To run last builded version of our project
```sh
stack run
```

To use our project you need input telegram bot API token.
You can find all necessary information about token creation process <a href=https://core.telegram.org/bots/faq#how-do-i-create-a-bot>here</a> 
```sh
Please enter telegram token:
<Your Telegram Bot API Token>
```
Enjoy!
