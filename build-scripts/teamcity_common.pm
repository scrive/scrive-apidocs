#
# Common routines for the TeamCity Build System.
#

package teamcity_common;

use strict;
use base 'Exporter';

our @EXPORT = qw( teamcity_escape teamcity_warning teamcity_error system_or_die open_or_die git_current_branch);
# TEAMCITY_GIT_PATH

sub teamcity_error {
  my ($arg) = @_;
  my $escaped = teamcity_escape($arg);
  print STDERR "##teamcity[message text='$escaped' status='ERROR']\n";
};

sub teamcity_warning {
  my ($arg) = @_;
  my $escaped = teamcity_escape($arg);
  print STDERR "##teamcity[message text='$escaped' status='WARNING']\n";
};

sub open_or_die {
  my $pid = open(my $fh, "-|", @_);
  if( $pid ) {
    return ($fh, $pid);
  }
  else {
    teamcity_error("Cannot execute: @_, $!");
    # Cannot die with a message here as that message is incompatible
    # with what TeamCity expects
    die;
  }
};

sub system_or_die {
  my @args = @_;
  print @args.join(" ") . "\n";
  my $result = system(@args);
  if( ($result>>8) > 0) {
    my $msg = "Cannot execute: " . join(" ", @args) . ": " . $!;
    teamcity_error($msg);
    # Cannot die with a message here as that message is incompatible
    # with what TeamCity expects
    die;
  }
  return $result;
};


sub teamcity_escape {
  my ($x) = @_;
  $x =~ s/\|/||/g;
  $x =~ s/\'/|'/g;
  $x =~ s/\n/|n/g;
  $x =~ s/\r/|r/g;
  $x =~ s/\[/|[/g;
  $x =~ s/\]/|]/g;

  return $x;
};

sub git_current_branch {
  my $git = $ENV{TEAMCITY_GIT_PATH} || "git";

  my ($fh, $pid) = open_or_die($git, "branch", "--no-color");
  my $branch;
  while( <$fh> ) {
    if( /^\* (.*)/ ) {
      $branch = $1;
      last;
    }
  }
  return $branch;
};

1;
