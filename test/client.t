use strict;
use warnings;

use Test::More;
use File::Temp qw(tempdir);
use File::Path qw(make_path);
use Cwd qw(abs_path);
use IPC::Open3;
use Symbol qw(gensym);
use JSON::PP qw(decode_json);

my $script = abs_path("bin/emacsc");
make_path(".tmp");
my $root = tempdir("client.XXXXXX", DIR => ".tmp", CLEANUP => 1);
$root = abs_path($root);
make_path("$root/bin", "$root/home");

for my $name ("emacsclient", "neomacsclient", "client ; with spaces") {
    open my $file, ">", "$root/bin/$name" or die $!;
    print {$file} <<'MOCK';
#!/usr/bin/perl
use strict;
use warnings;
use JSON::PP qw(encode_json);
open my $log, ">>", $ENV{CLIENT_TEST_LOG} or die $!;
print {$log} encode_json({program => $0, args => \@ARGV,
                         alternate => $ENV{ALTERNATE_EDITOR}}), "\n";
close $log;
if (@ARGV >= 2 && $ARGV[-2] eq "-e" && $ARGV[-1] eq "window-system") {
    print "neo\n";
    exit 0;
}
exit($ENV{CLIENT_TEST_STATUS} || 0);
MOCK
    close $file;
    chmod 0755, "$root/bin/$name" or die $!;
}

sub run {
    my ($env, @args) = @_;
    local %ENV = %ENV;
    delete @ENV{qw(EMACSCLIENT EMACS_SERVER_NAME EMACSCOPT ALTERNATE_EDITOR INSIDE_EMACS CLIENT_TEST_STATUS)};
    @ENV{keys %$env} = values %$env;
    $ENV{HOME} = "$root/home";
    $ENV{PATH} = "$root/bin:$ENV{PATH}";
    $ENV{CLIENT_TEST_LOG} = "$root/calls";
    unlink $ENV{CLIENT_TEST_LOG};
    my $err = gensym;
    my $pid = open3(undef, my $out, $err, $script, @args);
    my $stdout = do { local $/; <$out> };
    my $stderr = do { local $/; <$err> };
    waitpid $pid, 0;
    my $status = $? >> 8;
    my @calls;
    if (open my $log, "<", $ENV{CLIENT_TEST_LOG}) {
        @calls = map { decode_json($_) } <$log>;
        close $log;
    }
    return ($status, \@calls, $stdout, $stderr);
}

for my $env ({}, {EMACSCLIENT => ""}, {EMACSCLIENT => "neomacsclient"}) {
    my ($status, $calls) = run($env, "-e", "t");
    is($status, 0, "evaluation succeeds");
    is(scalar @$calls, 2, "client handles both backend query and evaluation");
    my $name = $env->{EMACSCLIENT} || "emacsclient";
    is_deeply([map { $_->{program} } @$calls], [("$root/bin/$name") x 2],
              "selected client is used consistently");
    is($calls->[1]{alternate}, "", "automatic startup remains enabled by default");
}

my ($status, $calls) = run({EMACSCLIENT => "$root/bin/client ; with spaces",
                           EMACS_SERVER_NAME => "from-env"},
                          "-g", "-s", "socket * [x]", "file ; with spaces");
is($status, 0, "client paths containing spaces and punctuation work");
is_deeply($calls->[0]{args}, ["-s", "socket * [x]", "-e", "window-system"],
          "backend query preserves the selected socket");
is_deeply($calls->[1]{args}, ["-s", "socket * [x]", "-c", "file ; with spaces"],
          "explicit socket overrides the environment and preserves argument boundaries");

($status, $calls) = run({EMACSCLIENT => "neomacsclient", ALTERNATE_EDITOR => "fallback"}, "-d");
is(scalar @$calls, 1, "daemon mode skips the backend query");
is($calls->[0]{alternate}, "fallback", "explicit alternate editor is preserved");

($status, $calls) = run({EMACSCLIENT => "neomacsclient", ALTERNATE_EDITOR => "fallback"}, "-k");
is($calls->[0]{alternate}, "false", "kill never starts an alternate editor");

for my $args (["-e", "t"], ["-p", "-e", "t"]) {
    ($status, $calls) = run({EMACSCLIENT => "neomacsclient", CLIENT_TEST_STATUS => 17}, @$args);
    is($status, 17, "client failure status is preserved");
    like($calls->[-1]{program}, qr{/neomacsclient\z}, "failure comes from the selected client");
}

($status, $calls) = run({EMACSCLIENT => "missing-client"}, "--help");
is($status, 0, "help does not require a client");
is(scalar @$calls, 0, "help never invokes a client");

done_testing;
