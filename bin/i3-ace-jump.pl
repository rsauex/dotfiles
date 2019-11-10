#!/usr/bin/env perl

use strict;
use warnings;
use AnyEvent::I3;
use v5.10;

use utf8;

use Data::Dumper;

my $i3 = i3();
$i3->connect->recv or die "Error connecting to i3";

my $current_ws = (grep { $_->{'focused'} == 1 } @{i3->get_workspaces->recv})[0]->{'name'};

sub get_current_workspace_tree {
    my %tree = %{shift @_};
    my $ws_name = shift @_;
    if ($tree{'type'} eq 'workspace') {
        if ($tree{'name'} eq $ws_name) {
            return \%tree;
        }
    } else {
        foreach (@{$tree{'nodes'}}) {
            my $found = get_current_workspace_tree($_, $ws_name);
            if ($found) {
                return $found;
            }
        }
    }
    return 0;
}

my %ws = %{get_current_workspace_tree(i3->get_tree->recv, $current_ws)};

sub get_all_windows {
    my %tree = %{shift @_};
    if (scalar(@{$tree{'nodes'}}) == 0 && scalar(@{$tree{'floating_nodes'}}) == 0) {
        my @result = ( $tree{'id'} );
        return \@result;
    } else {
        my @result = ( );
        foreach (@{$tree{'nodes'}}) {
            @result = ( @result, @{get_all_windows($_)} );
        }
        foreach (@{$tree{'floating_nodes'}}) {
            @result = ( @result, @{get_all_windows($_)} );
        }
        return \@result;
    }
}

my @windows = @{get_all_windows(\%ws)};

my @bars = @{i3->get_bar_config->recv};

my @hidden_bars = ();

foreach (@bars) {
    if (i3->get_bar_config($_)->recv->{'mode'} eq 'hide') {
        push(@hidden_bars, $_);
        @{i3->command("bar mode invisible $_")->recv}[0]->{success} or die 'command failed';
    }
}

my @marks = ('a'..'z');

for ( my $i = 0; $i < @windows && $i < @marks; $i++ ) {
    @{i3->command("[con_id=\"$windows[$i]\"] mark $marks[$i]")->recv}[0]->{success} or die 'command failed';
}

my $cv = AE::cv;

my %callbacks = (
    mode => sub {
        my %event = %{shift @_};
        if ($event{'change'} eq 'Ace_Jump_Mode') {
            return;
        }
        $cv->send;
    }
);

$i3->subscribe(\%callbacks)->recv->{success} or die 'command failed';

@{i3->command("mode \"Ace_Jump_Mode\"")->recv}[0]->{success} or die 'command failed';

$cv->recv;

for ( my $i = 0; $i < @windows && $i < @marks; $i++ ) {
    @{i3->command("[con_id=\"$windows[$i]\"] unmark")->recv}[0]->{success} or die 'command failed';
}

foreach (@hidden_bars) {
    @{i3->command("bar mode hide $_")->recv}[0]->{success} or die 'command failed';
}
