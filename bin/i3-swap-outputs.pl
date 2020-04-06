#!/usr/bin/env perl

use strict;
use warnings;
use AnyEvent::I3;
use v5.10;

use utf8;

my $i3 = i3();
$i3->connect->recv or die "Error connecting to i3";

sub i3_run_or_die {
    my $command = shift @_;
    @{i3->command($command)->recv}[0]->{success} or die "Command '$command' failed";
}

my @outputs = (grep { $_->{'active'} == 1 } @{i3->get_outputs->recv});
my $outputs_number = scalar @outputs;
if ($outputs_number < 2) {
    die "Not a multi-head setup";
}

my @workspaces = @{i3->get_workspaces->recv};

my @new_outputs;
if (defined $ARGV[0] && $ARGV[0] eq "-r") {
    @new_outputs = ( @outputs[1..($outputs_number - 1)], $outputs[0]);
} else {
    @new_outputs = ( $outputs[$outputs_number - 1], @outputs[0..($outputs_number - 2)]);
}

for my $i (0..($outputs_number - 1)) {
    my $output = $outputs[$i]->{'name'};
    my $new_output = $new_outputs[$i]->{'name'};
    foreach (grep { $_->{'output'} eq $output } @workspaces) {
        my $workspace = $_->{'name'};
        i3_run_or_die "[workspace=\"$workspace\"] move workspace to output $new_output";
    }
}

foreach (grep { $_->{'visible'} == 1 } @workspaces) {
    my $workspace = $_->{'name'};
    i3_run_or_die "workspace $workspace";
}

my $focused_workspace = (grep { $_->{'focused'} == 1} @workspaces)[0]->{'name'};
my $focused_output = (grep { $_->{'current_workspace'} eq $focused_workspace } @outputs)[0]->{'name'};

i3_run_or_die "focus output $focused_output";
