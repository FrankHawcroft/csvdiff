#!/usr/bin/perl -w
#
# csvdiff.pl
# ----------
#
# Compare two CSV files and output an HTML or text file which combines them and
# displays lines which are the same, different, inserted, or deleted.

use strict;
use warnings;

use Pod::Usage;
use Getopt::Long;
use HTML::Element;
use POSIX qw(ceil);

use FindBin; # see perlfaq8
use lib "$FindBin::Bin";

use Text::CSV_XS;
use Match;

# Callback for option processing:
sub process_cl_name;

sub read_csv_file_xs;
sub csv_dimensions;

sub normed_case;
sub get_cell_key;
sub exact_match;

sub for_display;
sub line_num_display;
sub action_separator;

# Callbacks for the matcher:
sub m_col_name_key;
sub m_col_score;

sub m_get_whole_row_key;
sub m_get_key_col_key;
sub m_match_score;

# Output functions:
# 1. Text output:
sub text_output_header;
sub text_output_rows;
sub text_output_footer;
# 2. HTML output:
sub html_output_header;
sub html_output_rows;
sub html_output_footer;
sub max_width; # TO DO: should be used by textual output too.
sub style_for_html;

my $l_file = "";
my $r_file = "";

# Optional parameters.
my $output_file = "";
my $html_output = 0; 
my $named_cols = 0; 
my $unordered = 0; 
my $ignore_case = 0; 

# TO DO: allow column names to be given for key cols if --named-columns
my $l_key_col = -1; # left hand key column index (0 based)
my $r_key_col = -1; # right hand key column index (0 based)

my $help = 0;
my $man = 0;

sub process_cl_name {
	my $t = shift;
	if($l_file eq '' && $r_file eq '') { $l_file = $t; }
	elsif($l_file ne '' && $r_file eq '') { $r_file = $t; }
	else { die "More than two filenames supplied - try --help\n" }
} 

exit(1) unless GetOptions("left-file=s" => \$l_file,
			  "right-file=s" => \$r_file,
			  "output-file=s" => \$output_file,
			  "html-output!" => \$html_output,
			  "named-columns!" => \$named_cols,
			  "unordered!" => \$unordered,
			  "ignore-case!" => \$ignore_case,
			  "left-key-col=i", => \$l_key_col,
			  "right-key-col=i", => \$r_key_col,
			  "help!" => \$help,
			  "man!" => \$man,
			  '<>' => \&process_cl_name);

pod2usage(-verbose => 2, -exitval => 0) if $man;
pod2usage(-verbose => 1, -exitval => 0) if $help;

die "Must supply two files to compare - try --help\n"
	if $l_file eq "" || $r_file eq "";

--$l_key_col if $l_key_col != -1;
--$r_key_col if $r_key_col != -1;

# Open output file if supplied:
if($output_file ne "") {
	open(OUTFILE, ">", $output_file) or die "Unable to open $output_file: $!.\n";
	select(OUTFILE);
}

sub read_csv_file_xs {
	my $file = shift;
	my $csv = Text::CSV_XS->new({ binary => 1, eol => $/ });
	open my $io, "<", $file or die "Unable to open $file: $!\n";
	my @all_row = ();
	while(my $row = $csv->getline($io)) {
		my @fields = @$row;
		push @all_row, [@fields];
	}
	close($io);
	return @all_row;
}

sub csv_dimensions(@) {
	my $height = @_;
	my $width = 0;
	foreach(@_) {
		my @cell = @{$_};
		$width = @cell if @cell > $width;
	}
	($width, $height);
}

# Read the CSV files to compare:
my @ll = read_csv_file_xs($l_file);
my @rl = read_csv_file_xs($r_file);

# Calculate maximum widths (in cells, not characters!) for HTML table alignment purposes.
# TO DO: support for text output too.
my ($max_width_l, $l_rows) = csv_dimensions(@ll); # CSV::dimensions(@ll);
my ($max_width_r, $r_rows) = csv_dimensions(@rl); # CSV::dimensions(@rl);

# Calculate key length per cell.
# TO DO: this doesn't allow for the size of data within cells, just the number
# of rows and columns.
use constant min_cell_key_length => 1;
use constant max_cell_key_length => 8;
use constant max_key_length => 256;

my $avg_cols = ceil(($max_width_l + $max_width_r) / 2);
$avg_cols = 1 if $avg_cols == 0;

my $cell_key_length = max_key_length / $avg_cols;
$cell_key_length = max_cell_key_length
	if $cell_key_length > max_cell_key_length;
$cell_key_length = min_cell_key_length
	if $cell_key_length < min_cell_key_length;

# Build column index map:
my %l_to_r_col = ();
my %r_to_l_col = ();

my @corresp_col = ();
if($named_cols) {
	@corresp_col = Match::match($ll[0], $rl[0], 0, \&m_col_name_key, \&m_col_score);
}
else {
	# TO DO: use map here -
	for(my $i = 0; $i < $max_width_l || $i < $max_width_r; $i++) {
		push @corresp_col, [$i, $i];
	}
}

die "Files are empty or have no columns in common!\n"
	if grep({$_->[0] != -1 && $_->[1] != -1} @corresp_col) == 0;

$l_to_r_col{$_->[0]} = $_->[1] foreach(@corresp_col);
$r_to_l_col{$_->[1]} = $_->[0] foreach(@corresp_col);

# Match the rows from the two files:
my @both = Match::match(\@ll, \@rl, !$unordered, 
	($l_key_col >= 0) ? \&m_get_key_col_key : \&m_get_whole_row_key, 
	\&m_match_score);

# Output the results:
my $htree = 0; # only used for HTML output
my ($output_header, $output_rows, $output_footer) = ($html_output) 
	? (\&html_output_header, \&html_output_rows, \&html_output_footer)
	: (\&text_output_header, \&text_output_rows, \&text_output_footer);

&$output_header();
&$output_rows($_->[0], $_->[1]) foreach @both;
&$output_footer();

close(OUTFILE) if $output_file ne "";

sub normed_case {
	my $s = shift;
	$ignore_case ? uc($s) : $s;
}

sub m_col_name_key {
	my ($name, $row_num, $side) = @_;
	return normed_case($name);
}

sub m_col_score {
	my ($l, $r) = @_;
	return ($l->[1] eq $r->[1]) ? 1 : -1; # keys
}

# Returns a comparison key for a single CSV row, for Match::match.
# $line is a reference to an array of cell values.
# TO DO: doesn't (yet) support the following:
# $row_num is the 'row number', which will not correspond to a line number if
# the file contains multi-line quoted fields (these are allowed in CSV format).
# This should be significant, if row order matters, in comparison (favour closer
# matches). Question is, how to factor it in ... use Algorithm::LCS?
sub m_get_whole_row_key {
	my ($line, $row_num, $side) = @_;

	my @field = canonical_order($side, @{$line});

	# Always include row number in key.
	# Even if it isn't used for comparison, we still want it for sorting and display.
	my $the_key = "";
	
	# TO DO: change length (and position?) sampled depending on total file sizes etc.  
	$the_key .= get_cell_key($_, $cell_key_length) foreach @field;

	return normed_case($the_key);
}

# Sorts the list of cells into the correct order for comparison.
# This is only significant if using named columns.  In that case, the cells
# are sorted based on the order of column names, removing any which don't have
# a corresponding column name in the other side.
sub canonical_order {
	my ($side, @field) = @_;

	my @sorted = ();
	foreach(my $i = 0; $i < @field; $i++) {
		if($side == 0 && exists($l_to_r_col{$i}) && $l_to_r_col{$i} > -1) {
			push @sorted, $field[$l_to_r_col{$i}];
		}
		elsif($side == 1 && exists($r_to_l_col{$i}) && $r_to_l_col{$i} > -1) {
			push @sorted,  $field[$i];
		}
		else {
			push @sorted, "";
		}
	}

	return @sorted;
}

# TO DO: change position sampled depending on field length etc. ...
sub get_cell_key {
	my ($cell, $sample_length) = @_;

	my $l = length($cell);

	# @s_len must equal max_cell_key_length!
	my @s_len = ([1, 0, 0], [1, 0, 1], [1, 1, 1], [2, 1, 1], [2, 1, 2],
		 [2, 2, 2], [3, 2, 2], [3, 2, 3]);
	my ($sl, $ml, $el);
	my $sli = $sample_length;

	do {
		($sl, $ml, $el) = @{$s_len[--$sli]};
	} while($sli > 0 && $sl + $ml + $el > $l);

	# Get the parts of the string for our key:
	my $a = substr($cell, 0, $sl); # beginning
	my $b = ($ml > 0) ? substr($cell, $l / 2 - $ml / 2, $ml) : ''; # middle
	my $c = ($el > 0) ? substr($cell, $l - $el) : ''; # end

	# Normalise the key to its desired length.  sprintf pads with spaces.
	my ($start, $middle, $end) = @{$s_len[$sample_length - 1]};
	my $fmt = "%-$start.$start" . 's' . "%-$middle.$middle" . 's' . "%-$end.$end" . 's';
	return sprintf($fmt, $a, $b, $c);
}

# Returns a comparison key for a single CSV row, for Match::match.
# Note that $line is a reference to an array of cell values.
# This version is used if a key column was supplied. ONLY the cell in that
# column will be used. The entire cell content is used. Row number is ignored.
sub m_get_key_col_key {
	my ($line, $row_num, $side) = @_;
	my @field = @{$line};
	# TO DO: should probably check for out of bounds here rather than just
	# leaving Perl to throw an error.
	my $the_key = $field[($side == 0) ? $l_key_col : $r_key_col];
	return normed_case($the_key);
}

# Match scoring, for Match::match.
# TO DO: row number significance and key column option ...
sub m_match_score {
	my ($l, $r) = @_;
	my $score = 0;

	# Ensure header row matches if using named columns:
	return 1000000 if $named_cols && $l->[0] == 0 && $r->[0] == 0;

	$l = $l->[1]; # key
	$r = $r->[1];

	# TO DO: option to make row num significant.
	# In this case, favour matching rows which are 'close' (within some
	# percentage of average row counts?) in files.
	
	# Require exact key match if key column number was supplied.
	# TO DO: hmmmm ... not sure about this now ... perhaps have --exact-match
	# option?

	for(my $i = 0; $i < length($l) && $i < length($r); $i++) {
		++$score if substr($l, $i, 1) eq substr($r, $i, 1);
	}

	# Must match in more than half the characters (on average).
	return ($score >= (length($l) + length($r)) / 4) ? $score : -1;
}

# NOTE: takes entire row arrays, not keys!
sub exact_match {
	my ($left, $right) = @_;
	my @lc = canonical_order(0, @{$left});
	my @rc = canonical_order(1, @{$right});
	return 0 if @lc != @rc;
	for(my $i = 0; $i < @lc; $i++) {
		return 0 if normed_case($lc[$i]) ne normed_case($rc[$i]);
	}
	return 1;
}

# Dummy - no header is currently printed for textual output.
sub text_output_header {
}

# TO DO: indicate cell-level differences/similarities.  How to do this in text?
# TO DO: fix funny display problem.
sub text_output_rows {
	my ($l, $r) = @_;

	my @l_row = ($l > -1) ? @{$ll[$l]} : ();
	my @r_row = ($r > -1) ? @{$rl[$r]} : ();
	
	my $lt = ($l > -1) ? for_display(@l_row) : "";
	my $rt = ($r > -1) ? for_display(@r_row) : "";

	my $separator = action_separator($l, $r, \@l_row, \@r_row);

	if($l != -1 && $r != -1) {
		print line_num_display($l) . ": $lt $separator " . line_num_display($r) . ": $rt\n";
	}
	elsif($l != -1) {
		print line_num_display($l) . ": $lt $separator\n";
	}
	else {
		# TO DO: will not align nicely!  Need to use max length of left, right
		my $right = "$separator " . line_num_display($r) . ": $rt\n";
		print ' ' x (length($right) - 2) . $right;
	}
}

# Dummy - no header is currently printed for textual output.
sub text_output_footer {
}

sub action_separator {
	my ($l, $r, $rlc, $rrc) = @_;
	return "|" if $l != -1 && $r != -1 && exact_match($rlc, $rrc);
	return "#" if $l != -1 && $r != -1;
	return "-" if $l != -1;
	return "+";
}

sub html_output_header {
	# The DOCTYPE comment is not supported by HTML::Element.

	#print '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"' .
	#' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">';

	$htree = HTML::Element->new('html', 
				'xmlns', 'http://www.w3.org/1999/xhtml');
	$htree->push_content(
			  ['head',
			   ['title', "csvdiff results: $l_file, $r_file"],
			   ['style', 
"* {font-family: sans-serif; }
.linenum { font-weight: bold; text-align: center; }
.result { font-weight: bolder; text-align: center; }
.changed { color: red; } 
.added { color: green; } 
.deleted { color: blue; }
.same { color: black; }"
			   ]],
			  ['body',
			   ['table']
			   ]);
}

# Output two rows as HTML.  Colours are used to indicate whether cells
# have been added (i.e. outside width of other row, or non-empty when empty
# in other row), removed, or changed.
sub html_output_rows {
	my ($l, $r) = @_;

	my @l_row = ($l > -1) ? @{$ll[$l]} : ();
	my @r_row = ($r > -1) ? @{$rl[$r]} : ();

	my $is_header = $l == 0 && $r == 0 && $named_cols;

	# Get row text for testing 'exact' equality:
	#my $lt = ($l > -1) ? for_display(@l_row) : "";
	#my $rt = ($r > -1) ? for_display(@r_row) : "";

	# TO DO: this is pretty ugly - encode the colour used to indicate the
	# status of the cell (different, matches, added, removed) using a character
	# prepended to the cell's content.
	# First for the left row ...
	for(my $li = 0; $li < @l_row; $li++) {
		my $code = "";
		my $ri = (exists $l_to_r_col{$li}) ? $l_to_r_col{$li} : -1;
		if($ri >= @r_row || $ri == -1 || $r_row[$ri] eq "") { $code = "r"; }
		elsif(normed_case($l_row[$li]) ne normed_case($r_row[$ri])) { $code = "d"; }
		else { $code = "m"; }
		$l_row[$li] = $code . $l_row[$li];
	}
	
	# ... then for the right one.
	for(my $ri = 0; $ri < @r_row; $ri++) {
		my $code = "";
		my $li = (exists $r_to_l_col{$ri}) ? $r_to_l_col{$ri} : -1;
		if($li >= @l_row || $li == -1 || $l_row[$li] =~ /^.$/) { $code = "a"; }
		elsif(normed_case(substr($l_row[$li], 1)) ne normed_case($r_row[$ri])) { $code = "d"; }
		else { $code = "m"; }
		$r_row[$ri] = $code . $r_row[$ri];
	}

	# Create dummy row(s) if no match(es):
	@l_row = map("m", 1 .. $max_width_l) if $l == -1;
	@r_row = map("m", 1 .. $max_width_r) if $r == -1;

	my $separator = action_separator($l, $r, \@l_row, \@r_row);
	# TO DO: need to handle newlines ... replace with <br/>
	# TO DO: for cells which are empty, and have changed, change bg (block) colour to draw attention to the fact?
	$htree->find('table')->push_content(
		['tr',
		 ['th', {'class' => 'linenum'}, line_num_display($l)],
		 map([$is_header ? 'th' : 'td', 
			  {'class' => style_for_html(substr($_, 0, 1))}, substr($_, 1)], 
			 @l_row),
		 ['th', {'class' => 'result'}, $separator],
		 ['th', {'class' => 'linenum'}, line_num_display($r)],
		 map([$is_header ? 'th' : 'td',
			  {'class' => style_for_html(substr($_, 0, 1))}, substr($_, 1)], 
			 @r_row)
		]);
}

# TO DO: this is somewhat misleadingly named, since it outputs the whole HTML tree.
sub html_output_footer {
	print $htree->as_HTML();
	$htree = $htree->delete();
}

sub style_for_html {
	my $status = shift;
	return "changed" if $status eq "d"; # different
	return "added" if $status eq "a"; # added
	return "deleted" if $status eq "r"; # removed
	return "same" if $status eq "m"; # matches (same)
	die "internal error - unknown style code for HTML output";
}

sub for_display {
	my $csv = Text::CSV_XS->new({ binary => 1, eol => $/ });
	my $status = $csv->combine (@_);
	die "CSV_XS combine() error: " . $csv->error_input() . "\n" unless $status;
	my $row_text = $csv->string();
	#$row_text =~ s/\n/\\n/sg; # escape newlines
	chomp($row_text);
	return $row_text;
}

# TO DO: zero/one based line numbers option.
sub line_num_display {
	my $line_nbr = shift;
	return ($line_nbr == -1) ? "" : 1 + $line_nbr;
}

__END__

=head1 NAME

csvdiff

=head1 SYNOPSIS

csvdiff [options] [--left-file] file1 [--right-file] file2

  Required Parameters:
	[--left-file] file1   the first ('base') file
	[--right-file] file2  the second ('revised') file

  Options:
	--output-file name writes output to the specified file, not stdout
	--html-output      produces an HTML-format report
	--named-columns    uses named columns (column names in first rows)
	--unordered        ignores order of rows in files
	--ignore-case      ignores case differences when comparing text
	--left-key-col n   uses the specified column number as key in comparison
	--right-key-col m  uses the specified column number as key in comparison
	--help             display brief help message and exit
	--man              display full documentation and exit

=head1 OPTIONS

=over 8

=item B<file1 (--left-file)>

Specifies the left-hand or 'base' file.

=item B<file2 (--right-file)>

Specifies the right-hand or 'revised' file.

=item B<--output-file name>

Writes output (CSV or HTML format) to the specified file.  Default is stdout.

=item B<--html-output>

Produces an HTML file containing the results of the comparison.  This is essentially an HTML <table>.  An advantage of this format is that colours are used to highlight results, on a cell-level.  The default is to produce a 'plain text' (CSV) format.

=item B<--named-columns>

Uses named columns when comparing the files.  The column names must appear in the first row of each file.  For best results, these columns should match in the two files.  TO DO: document/decide (1) what happens when there are non-matching columns - do I report anything; (2) what happens to these in the actual comparison.

=item B<--unordered>

Ignores the order of the rows in the files, attempting to match rows based entirely on their content, rather than their position in the file.

=item B<--ignore-case>

Ignores case differences when comparing text in cells.  The default is to treat case differences as significant (so 'HELLO' will be reported as not matching 'Hello', for example).

=item B<--left-key-col n> B<--right-key-col m>

Use the specified column(s) as keys.  Only these columns will be used in matching rows - other cells will be ignored, although differences in these cells will still be reported in the output.  Column numbers are 1-based.

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=back

=head1 DESCRIPTION

B<csvdiff> is a utility to assist in comparing CSV files.  It will read the two files and produce an output report containing matching and non-matching rows from the files.

=cut
