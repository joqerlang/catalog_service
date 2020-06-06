all:
	rm -rf app_config catalog node_config  logfiles *_service include *~ */*~ */*/*~;
	rm -rf */*.beam;
	rm -rf *.beam erl_crash.dump */erl_crash.dump */*/erl_crash.dump;
	cp src/*.app ebin;
	erlc -I ../include -o ebin src/*.erl;
doc_gen:
	rm -rf  node_config logfiles doc/*;
	erlc ../doc_gen.erl;
	erl -s doc_gen start -sname doc
git:
	# start lib_service
	rm -rf *_service ../lib_service/ebin/* ../lib_service/test_ebin/* ../lib_service/erl_crash.dump catalog.info;
	cp catalog.info_git catalog.info
	cp ../lib_service/src/*app ../lib_service/ebin;
	erlc -I ../include -o ../lib_service/ebin ../lib_service/src/*.erl;
	rm -rf *.beam ebin/* test_ebin/* erl_crash.dump;
	cp src/*app ebin;
	erlc -I  ../include -D local -o ebin src/*.erl;
	erlc -I ../include -D local -D git -o test_ebin test_src/*.erl;
	erl -pa ../lib_service/ebin -pa ebin -pa test_ebin -s master_service_tests start -sname master_git_test
dir:
	# start lib_service
	rm -rf node_config logfiles latest.log  *_service ../lib_service/ebin/* ../lib_service/test_ebin/* ../lib_service/erl_crash.dump catalog.info;
	#cp catalog.info_dir catalog.info
	# lib_service
	cp ../lib_service/src/*app ../lib_service/ebin;
	erlc -I ../include -o ../lib_service/ebin ../lib_service/src/*.erl;
	# log_service
	cp ../log_service/src/*app ../log_service/ebin;
	erlc -I ../include -o ../log_service/ebin ../log_service/src/*.erl;
	# dns_service
	#cp ../dns_service/src/*app ../dns_service/ebin;
	#erlc -I ../include -o ../dns_service/ebin ../dns_service/src/*.erl;
	rm -rf *.beam ebin/* test_ebin/* erl_crash.dump;
	cp src/*app ebin;
	erlc -I  ../include -D local -o ebin src/*.erl;
	erlc -I ../include -D local -D dir -o test_ebin test_src/*.erl;
	erl -pa ../*/ebin -pa ebin -pa test_ebin -s catalog_service_tests start -sname catalog_dir_test
