#!/usr/bin/perl -w
# telnet_redboot.pl - ./upslug --reset;arping -f 192.168.0.1;telnet 192.168.0.1 9000

use Net::Telnet ();

my $host = $ARGV[0] or die "Syntax: $0 <host>\n";

 system("while true; do fping -t200 $host && break; done");

 my $t = new Net::Telnet (Port => 9000, Timeout => 30);
 if(!defined($t)){
     print "new Net::Telnet failed\n";
     exit(1);
 } # if
 my $ok;
 $ok = $t->errmode("return");
 $ok = $t->open($host);
 if(!defined($ok)){
     print "open('$host') failed\n";
     $t->close();
     exit(1);
 } # if

 # == Executing boot script in 1.930 seconds - enter ^C to abort

 my $line;
 while($line = $t->getline()){
        $line =~ s/[\r\n]//;
        print "-> $line\n";
        if($line =~ m/enter \^C to abort/){
                $t->put(chr(3)); # send ^C
                print "<- \^C\n";
                $t->close();
                sleep(1);
                system("telnet $host 9000");
                exit(0);
        } # if
 } # while
 $t->close();
