# include ../Makefile

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

chki:
	stack install :top-repo2 --file-watch	

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
