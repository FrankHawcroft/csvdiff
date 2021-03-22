# Array matching module. Useful for matching elements from two arrays
# according to some criterion, which can be defined by generating and then
# comparing a key value for each element.

package Match;

use strict;
use warnings;

use POSIX qw(ceil floor);

BEGIN {
	use Exporter   ();
	our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

	# set the version for version checking
	$VERSION     = 0.1;

	@ISA         = qw(Exporter);
	@EXPORT      = qw();
	%EXPORT_TAGS = ( ); 

	@EXPORT_OK   = qw(&match &test_me);
}
our @EXPORT_OK;

# The key is just the element ... second and third parameters (row number, side)
# are ignored.
my $def_key = sub {
	return shift;
};

# Simple matching function - only exact key matches count.
my $def_cmp = sub {
	my ($a, $b) = @_;
	# Second element in the list is the key.
	return ($a->[1] eq $b->[1]) ? 1 : -1;
};

# $side: 0 = left, 1 = right.
my $get_keys = sub {
	my ($get_key, $ordered, $side, @a) = @_;
	my $row_num = 0;
	my @key = ();
	foreach(@a) {
		# Save list of: original row number, key, contents of row.
		push @key, [$row_num, &$get_key($_, ($ordered) ? $row_num : -1, $side), $_];
		++$row_num;
	}
	return @key;
};

# The heuristic 'probe distance'.  This is made up out of thin air, a bit, but
# based on the vague principles that -
# 1. If the two arrays are of similar length, then we won't need to probe too
#    far, usually.
# 2. If the two arrays are of quite dissimilar lengths, we'll need to probe
#    further.
my $max_probe_dist = sub {
	my ($lt, $rt) = @_;
	return abs($lt - $rt) + ceil(sqrt(($lt + $rt) / 2));
};

# Helper function for sorting array of matches.
# See match() for interpretation of parameters.
# TO DO: the below logic is based on trial-and-error largely.
# It could be optimised somewhat.
my $display_order = sub {
	if($a->[0] != -1 && $a->[1] != -1) {
		if($b->[0] != -1 && $b->[1] != -1) {
			# Minimise difference in 'distance':
			my $l_diff = abs($a->[0] - $b->[0]);
			my $r_diff = abs($a->[1] - $b->[1]);
			return ($l_diff <= $r_diff) ? $a->[0] <=> $b->[0] : $a->[1] <=> $b->[1];
		}
		elsif($b->[0] != -1) {
			return $a->[0] <=> $b->[0];
		}
		else { # $b->[0] == -1
			return $a->[1] <=> $b->[1];
		}
	}
	elsif($a->[0] != -1) {
		return ($b->[0] != -1) ? $a->[0] <=> $b->[0] : -1;
	}
	else { # $a->[0] == -1
		return ($b->[1] != -1) ? $a->[1] <=> $b->[1] : 1;
	}
};

# TO DO:
# Needs to support -
# -k option - i.e. key column can be passed in somehow.
# TO DO: document!
sub match {
	my ($a1, $a2, $ordered, $get_key, $cmp_elts) = @_;

	# Extract the two arrays:
	# TO DO: somewhat inefficient I'm sure - perhaps should keep as refs.
	my @ll = @{$a1};
	my @rl = @{$a2};

	# Generate key and element lists for the two arrays:
	# Each elt is a triplet: [row num, key, whole row]
	my @lk = &$get_keys($get_key, $ordered, 0, @ll);
	my @rk = &$get_keys($get_key, $ordered, 1, @rl);

	# Sort the key/elt lists, unless order is significant.
	# Sorting is simply on (string) key values.
	if(!$ordered) {
		@lk = sort {$a->[1] cmp $b->[1]} @lk;
		@rk = sort {$a->[1] cmp $b->[1]} @rk;
	}

	# Perform the matching:
	my %l_to_r = ();
	my %r_to_l = ();

	my $probe_dist = &$max_probe_dist(scalar @lk, scalar @rk);

	for(my $l_idx = 0; $l_idx < @lk; $l_idx++) {
		# TO DO: 
		# - doesn't allow for exact matches vs. key-based matches.
		# - this is N * M - doesn't sort arrays by key, then probe within a
		# limited number of positions, or anything like that.
		# TO DO: make use of max_probe_dist.

		my $l_key = $lk[$l_idx];
		my $best_score = -1; # TO DO: set to INT_MIN or something like that??
		my $r_match_idx = -1;

		# TO DO: not sure about f.p.-ness of this ...
		my $start_probe_idx = floor(($l_idx / @lk) * @rk);
		my $bottom_limit = $start_probe_idx - ceil($probe_dist / 2);
		my $top_limit = $start_probe_idx + ceil($probe_dist / 2);

		# Search for the best match in the right hand list, within our probe
		# limit:
		for(my $r_idx = ($bottom_limit > 0) ? $bottom_limit : 0;
		  $r_idx < @rk && $r_idx < $top_limit; $r_idx++) {
			unless(exists $r_to_l{$r_idx}) {
				my $score = &$cmp_elts($l_key, $rk[$r_idx]);
				if($score > $best_score) {
					$best_score = $score;
					$r_match_idx = $r_idx;
				}
			}
		}

		if($r_match_idx >= 0) {
			$l_to_r{$l_idx} = $r_match_idx;
			$r_to_l{$r_match_idx} = $l_idx;
		}
	}

	# Build table of results:
	my @both = ();
	push @both, [$lk[$_]->[0], $rk[$l_to_r{$_}]->[0]] foreach(keys %l_to_r);
	for(my $i = 0; $i < @lk || $i < @rk; $i++) {
		push @both, [$lk[$i]->[0], -1] if $i < @lk && !(exists $l_to_r{$i});
		push @both, [-1, $rk[$i]->[0]] if $i < @rk && !(exists $r_to_l{$i});
	}

	# By default, sort results into a generally OK 'diff display' order:
	return sort $display_order @both;
}

# Test bed for the matcher:
sub test_me {
	my @l1 = ('a', 'b', 'c', 'd', 'e');
	my @l2 = ('a', 'e', 'd', 'c', 'g', 'k');

	# TO DO: match would be nicer if it used -
	# 1. default parameter values ($def_key and #def_cmp)
	# 2. named parameters (as referred to in the perlmod? man page)
	my @results = match(\@l1, \@l2, 1, $def_key, $def_cmp);

	foreach(@results) {
		my $li = $_->[0]; # left index
		my $ri = $_->[1]; # right index
		my $left = ($li > -1) ? $l1[$li] : "_";
		my $right = ($ri > -1) ? $l2[$ri] : "_";
		print "$li $left : $ri $right\n";
	}
}

END { } 

1;
