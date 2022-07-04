# HaskellBot
Grow your own Haskeller!
Telegram chat-bot for fun. The idea is to grow your own ”haskeller” with the following chat commands: /change_name, /grow, /status, /rank
- `/change_name` - to change the name of your haskeller
- `/grow` - to grow by 1 IQ of your “haskeller” and see the current IQ
- `/rank` - to see the current rank (each 10 IQ, the rank changes to the new one).
- `/info` - to see all the characteristics of your “haskeller” (name, IQ, rank)
<p>The initial state of “haskeller”:</p>
    <ul>
  <li>IQ = 0</li>
  <li>name = “”</li>
  <li>rank = "Newbie"</li>
    </ul>


# Building
```
git clone https://github.com/Legolass322/HaskellBot
```
#### Stack 
```sh
stack build
stack run
Please enter telegram token:
<Your Telegram Bot API Token>
```
Enjoy!
