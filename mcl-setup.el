
(defconst mcl-dir
  "/Volumes/Mac OS 9/Applications (Mac OS 9)/MCL 4.3.1 \"Demo Version\"")

(setq corba-name-service
      (expand-file-name "NameService" mcl-dir))
(setq corba-interface-repository
      (zopetalk-get-http "pentax.lst" 80 "/InterfaceRepository"))
