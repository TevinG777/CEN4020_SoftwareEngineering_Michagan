1) test1.txt - View multiple stored messages for Martin (messages exist from Tevin and Opal)
2) test2.txt - View Tevin's inbox when the [MESSAGES] block exists but is empty to ensure "You have no messages at this time." is shown.
3) test3.txt - View Marko's inbox when the profile file has no [MESSAGES] section at all and confirm the fallback still shows the "no messages" prompt.
4) test4.txt - Enter the Messages menu as Martin, provide an invalid option first, then select "View my messages" to validate the looped prompt and error handling message.
5) test5.txt - View Martin's messages twice in the same session (re-entering the Messages menu) to ensure the feature can be invoked 