(defproject audsk "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]
		 [overtone "0.6.0"]
		 [rosado.processing "1.1.0"]
		 [clj-tuio "0.0.4-SNAPSHOT"]]
  :jvm-opts ["-Xshare:off"
 	     "-Djava.library.path=/Applications/Processing.app/Contents/Resources/Java/modes/java/libraries/opengl/library/macosx"
	     "-d32"
	     ;; "-Djava.library.path=/Applications/Processing.app/Contents/Resources/Java/modes/java/libraries/opengl/library/macosx/lib"
	     ])


