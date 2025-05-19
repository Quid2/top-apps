# include ../Makefile

deploy:
	# git push
	- n1 "sudo killall --quiet --signal SIGKILL top-repo"
	n1 "cd /home/ubuntu/repo/top-apps;git pull;cabal install --installdir ./bin"

	# one of
	# n1 "cd /home/ubuntu/repo/top-apps;sudo bin/top-repo"

	n1 "cd /home/ubuntu/repo/top-apps;sudo bin/top-repo </dev/null >/dev/null 2>&1 &"
	n1 "ps aux | grep top"

dev:
	stack build top-apps:lib --fast --file-watch

repl:
	#stack ghci --main-is top-apps:exe:qq
	stack ghci top-apps:lib

APP=qq
APP=top-temperature

id:
	ghcid --command="stack ghci --main-is top-apps:exe:$(APP) --ghc-options -fno-warn-missing-signatures --ghc-options  -fno-warn-unused-top-binds"

id-test:
	ghcid --command="stack ghci --main-is top-apps:exe:qq --ghc-options -fno-warn-missing-signatures --ghc-options  -fno-warn-unused-top-binds" --test tst	

i:
	#stack build :top-test --file-watch
	stack build :top-repo --file-watch
	#  --fast	

r:
	stack run top-test u2zjoq8vfcqyptkfdm9ua73tggp9b5 a6y7czdaxd36y1uk7e2hruvx7qooy5

run:
	# tail -f ~/.top-repo2/log/debug.txt
	#stack run top-repo2 "MyConfig{logPriority=WARNING,logStdOut=False}"
	stack run top-repo2 "MyConfig{logPriority=DEBUG,logStdOut=True}"
	#stack run top-temperature "MyConfig{debugLevel=DEBUG}"
	stack run top-test

qq: 
	stack install :qq --file-watch --exec 'qq purescript --dir  "/Users/titto/workspace/top-apps-purs/src/"'

t:
	# stack run qq -- blob --srcFile hello.txt
	 stack install :qq
	 # echo BLOB{encoding= NoEncoding ,content= [72,101,108,108,111,32,87,111,114,108,100,33] } | qq unblob --file  qq-tutorial/hello2.txt
	 # qq blob --file  qq-tutorial/hello.txt > hello
	 # qq unblob --file  qq-tutorial/hello2.txt < hello
	 qq blob --file  qq-tutorial/hello.txt | cat - | qq unblob --file  qq-tutorial/hello2.txt
	 # ls -la qq-tutorial

# encode a PushoverId value
# copy and paste value and 
# complete input wth ctrl-d
enc:
	gpg --encrypt --recipient quidagent@gmail.com --output PushoverId.gpg

# This can now be decrypted on every computer with the quidagent@gmail.com private key
dec:
	gpg --decrypt PushoverId.gpg

# To transfer the required key onto another computer
TARGET=n2
mov:
    # Export a key (done already)
	# gpg --export-secret-key quid2 > quidagent.key

	# copy it to target system
	scp ~/.ssh/quidagent.key ubuntu@${TARGET}.quid2.org:~/.ssh

	# Import in target system	
	${TARGET} "gpg --import ~/.ssh/quidagent.key"

	# Verify that key is now present
	${TARGET} "gpg --list-keys"

	# Now secrets can be decrypted with 
	# gpg --openpgp --decrypt PushoverId.gpg	

# make new name="time"
new:
	stack new top-$(name) ./app