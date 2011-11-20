#!/usr/bin/perl -w

use strict;
use warnings;

# For Windows
#use lib 'C:/path/to/lib';

# For Linux
use lib '/path/to/lib';

#=======================================================================
# "prog" document source for spidering web servers
#
# For documentation, type:
#
#       perldoc spider.pl
#
#    Copyright (C) 2009 Sami Barakat sami@sbarakat.co.uk
#
#    Original code sourced from the Swish-e project 
#     http://swish-e.org/
#
#    This program is free software; you can redistribute it and/or
#    modify it under the terms of the GNU General Public License
#    as published by the Free Software Foundation; either version
#    2 of the License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    The above lines must remain at the top of this program
#-----------------------------------------------------------------------

#=======================================================================
# TODO
#
# max_depth never increments
# Read robots meta tag and apply nofollow,noindex
# Allow all config options to be passed from command line
#-----------------------------------------------------------------------

$HTTP::URI_CLASS = "URI";   # prevent loading default URI::URL
                            # so we don't store long list of base items
                            # and eat up memory with >= URI 1.13
use LWP::RobotUA;
use HTML::LinkExtorRel;
#use HTML::RelExtor;
use HTML::Tagset;
use Time::HiRes qw(gettimeofday tv_interval);
use Config::Tiny;

use constant VERSION        => '1.0b';
use constant MAX_SIZE       => 5_000_000;   # Max size of document to fetch
use constant MAX_WAIT_TIME  => 30;          # request time.
use constant MAX_REDIRECTS  => 20;  # keep from redirecting forever

use vars '$bit';
use constant DEBUG_URL      => $bit = 1;  # print out every URL processes
use constant DEBUG_SKIPPED  => $bit <<= 1;  # didn't index for some reason
use constant DEBUG_LINKS    => $bit <<= 1;  # prints links as they are extracted

my %DEBUG_MAP = (
    url         => DEBUG_URL,
    skipped     => DEBUG_SKIPPED,
    links       => DEBUG_LINKS,
);

# Valid config file options
my @config_options = qw(
    agent
    base_url
    delay_sec
    email
    ignore_robots_file
    keep_alive
    link_tags
    max_depth
    max_indexed
    max_size
    max_time
    max_wait_time
    remove_leading_dots
    same_hosts
    use_cookies
    use_head_requests
    use_md5
    validate_links
);
my %valid_config_options = map { $_ => 1 } @config_options;

# This is not the right way to do this.
sub UNIVERSAL::host { '' };
sub UNIVERSAL::port { '' };
sub UNIVERSAL::host_port { '' };
sub UNIVERSAL::userinfo { '' };

#-----------------------------------------------------------------------

print STDERR "Web Spider (v".VERSION.")\n";
print STDERR "Copyright (C) 2009 Sami Barakat <sami\@sbarakat.co.uk>\n\n";

# Default command line parameters
my $results_file = "results.txt";
my $csv_file = "results.csv";
my $use_csv = 0;

# Read command line parameters
foreach my $num (0 .. $#ARGV)
{
    if ($ARGV[$num] =~ m/--csv=(.+)/)
    {
        $csv_file = $1;
        $use_csv = 1;
    }
    if ($ARGV[$num] =~ m/-csv/)
    {
        $use_csv = 1;
    }
    if ($ARGV[$num] =~ m/-h/)
    {
        print_usage();
        exit();
    }
}

# Test output files can be opened and clear them
open(DAT, ">$results_file") || die("Error: Cannot open file $results_file");
close(DAT);

if ($use_csv)
{
    open(DAT, ">$csv_file") || die("Error: Cannot open file $csv_file");
    printf DAT ("Timestamp,Status,Size,Load Time,Parent Page,URL,Duplicate URL\n");
    close(DAT);
}

print STDERR " Reading parameters from 'spider.conf'\n";

my $Config = Config::Tiny->new();
$Config = Config::Tiny->read("spider.conf");

my $abort;
local $SIG{HUP} = sub { warn "Caught SIGHUP\n"; $abort++ } unless $^O =~ /Win32/i;

my %visited;
my %validated;
my %bad_links;
my $server;

# To weed out
$server->{debug} = 0;

# Read config options
# Required
$server->{base_url} = $Config->{_}->{base_url};
@{$server->{same_hosts}} = split(/,/, $Config->{_}->{same_hosts})
    if $Config->{_}->{same_hosts};

# Optional
$server->{agent} = $Config->{_}->{agent} || 'spider http://example.com/';
$server->{email} = $Config->{_}->{email} || 'spider@domain.invalid';
$server->{delay_sec} = $Config->{_}->{delay_sec};
$server->{keep_alive} = $Config->{_}->{keep_alive};
$server->{ignore_robots_file} = $Config->{_}->{ignore_robots_file};
$server->{ignore_nofollow} = $Config->{_}->{ignore_nofollow};
$server->{use_md5} = $Config->{_}->{use_md5};
$server->{max_size} = $Config->{_}->{max_size} || MAX_SIZE;
$server->{max_wait_time} = $Config->{_}->{max_wait_time} || MAX_WAIT_TIME;
$server->{max_depth} = $Config->{_}->{max_depth};
$server->{max_indexed} = $Config->{_}->{max_indexed};
$server->{max_time} = $Config->{_}->{max_time} * 60 + time
    if $Config->{_}->{max_time};

@{$server->{link_tags}} = split(/,/, $Config->{_}->{link_tags});

$server->{link_tags} = ['a'] unless ref $server->{link_tags} eq 'ARRAY';
$server->{link_tags_lookup} = { map { lc, 1 } @{$server->{link_tags}} };

# Validate
die "Error: You must specify 'base_url' in your spider config settings\n" unless $server->{base_url};
die "Error: max_size parameter '$server->{max_size}' must be a number\n" unless $server->{max_size} =~ /^\d+$/;
die "Error: max_wait_time parameter '$server->{max_wait_time}' must be a number\n" unless $server->{max_wait_time} =~ /^\d+$/;
die "Error: max_depth parameter '$server->{max_depth}' must be a number\n" unless $server->{max_depth} =~ /^\d+/;

# Lame Microsoft
$URI::ABS_REMOTE_LEADING_DOTS = $server->{remove_leading_dots} ? 1 : 0;

#link_tags           => [qw/ a frame /],
#test_url            => sub {  $_[0]->path !~ /\.(?:gif|jpeg|png)$/i },
#test_response       => $response_sub,
#use_head_requests   => 1,  # Due to the response sub

my $start = time;

require "HTTP/Cookies.pm" if $server->{use_cookies};
require "Digest/MD5.pm" if $server->{use_md5};

# set starting URL, and remove any specified fragment
my $uri = URI->new($server->{base_url});
$uri->fragment(undef);

if ($uri->userinfo)
{
    die "Can't specify parameter 'credentials' because base_url defines them\n"
        if $server->{credentials};
    $server->{credentials} = $uri->userinfo;
    $uri->userinfo(undef);
}

print STDERR " Starting to spider: $uri\n\n";

# set the starting server name (including port) -- will only spider on server:port

# All URLs will end up with this host:port
$server->{authority} = $uri->canonical->authority;

# All URLs must match this scheme
$server->{scheme} = $uri->scheme;

# Now, set the OK host:port names
$server->{same} = [ $uri->canonical->authority || '' ];

push @{$server->{same}}, @{$server->{same_hosts}} if ref $server->{same_hosts};
$server->{same_host_lookup} = { map { $_, 1 } @{$server->{same}} };

# get a user agent object
my $ua;

# set the delay
unless (defined $server->{delay_sec})
{
    if (defined $server->{delay_min} && $server->{delay_min} =~ /^\d+\.?\d*$/)
    {
        # change if ever move to Time::HiRes
        $server->{delay_sec} = int ($server->{delay_min} * 60);
    }

    $server->{delay_sec} = 5 unless defined $server->{delay_sec};
}
$server->{delay_sec} = 5 unless $server->{delay_sec} =~ /^\d+$/;

if ($server->{ignore_robots_file})
{
    $ua = LWP::UserAgent->new;
    return unless $ua;
    $ua->agent($server->{agent});
    $ua->from($server->{email});
}
else
{
    $ua = LWP::RobotUA->new($server->{agent}, $server->{email});
    return unless $ua;
    $ua->delay(0);  # handle delay locally.
}

# If ignore robots files also ignore meta ignore <meta name="robots">
# comment out so can find http-equiv charset
# $ua->parse_head(0) if $server->{ignore_robots_file} || $server->{ignore_robots_headers};

# Set the timeout - used to only for windows and used alarm, but this
# did not always works correctly.  Hopefully $ua->timeout works better in
# current versions of LWP (before DNS could block forever)

$ua->timeout($server->{max_wait_time});

$server->{ua} = $ua;  # save it for fun.

$ua->cookie_jar(HTTP::Cookies->new) if $server->{use_cookies};

if ($server->{keep_alive})
{
    if ($ua->can('conn_cache'))
    {
        my $keep_alive = $server->{keep_alive} =~ /^\d+$/ ? $server->{keep_alive} : 1;
        $ua->conn_cache({ total_capacity => $keep_alive });
    }
    else
    {
        delete $server->{keep_alive};
        warn "Can't use keep-alive: conn_cache method not available\n";
    }
}

# Disable HEAD requests if there's no reason to use them
# Keep_alives is questionable because even without keep alives
# it might be faster to do a HEAD than a partial GET.
if ($server->{use_head_requests} && !$server->{keep_alive} || $server->{max_size})
{
    #warn 'Option "use_head_requests" was disabled.\nNeed keep_alive and max_size options\n';
    #delete $server->{use_head_requests};
}

# uri, parent, depth
eval { spider($server, $uri) };
printf STDERR ("\r%104s\r%s", " ", $@) if $@;

# Free up LWP to avoid CLOSE_WAITs hanging
delete $server->{ua};

$start = time - $start;
$start++ unless $start;

my $max_width = 0;
my $max_num = 0;
for (keys %{$server->{counts}})
{
    $max_width = length if length > $max_width;
    my $val = commify($server->{counts}{$_});
    $max_num = length $val if length $val > $max_num;
}

print STDERR "\nSummary for: $server->{base_url}\n";

for (sort keys %{$server->{counts}})
{
    printf STDERR "%${max_width}s: %${max_num}s  (%0.1f/sec)\n",
        $_,
        commify($server->{counts}{$_}),
        $server->{counts}{$_}/$start;
}

if (%bad_links)
{
    print STDERR "\nBad Links:\n\n";
    foreach my $page (sort keys %bad_links)
    {
        print STDERR "On page: $page\n";
        printf(STDERR " %-40s  %s\n", $_, $validated{$_}) for @{$bad_links{$page}};
        print STDERR "\n";
    }
}

#=======================================================================
# spider()
#
# Non recursive spidering
#
# Had problems with some versions of LWP where memory was not freed
# after the URI objects went out of scope, so instead just maintain
# a list of URI.
#-----------------------------------------------------------------------
sub spider
{
    my ($server, $uri) = @_;

    # Validate the first link, just in case
    return unless check_link($uri, $server, '', '(Base URL)');

    my @link_array = [ $uri, '', 0 ];

    while (@link_array)
    {
        die $server->{abort} if $abort;

        my ($uri, $parent, $depth) = @{shift @link_array};

        delay_request($server);

        # Delete any per-request data
        delete $server->{_request};

        my $new_links = process_link($server, $uri->clone, $parent, $depth);

        if ($new_links)
        {
            if (ref($new_links) eq 'ARRAY')
            {
                push @link_array, map { [ $_, $uri, $depth + 1 ] } @$new_links;
            }
            else
            {
                unshift @link_array, map { [ $_, $uri, $depth + 1 ] } $new_links;
            }
        }
    }
}

#=======================================================================
# delay_request()
#
# Delay a request based on the delay time
#-----------------------------------------------------------------------
sub delay_request
{
    my ($server) = @_;

    # Here's a place to log the type of connection
    if ($server->{keep_alive_connection})
    {
        $server->{counts}{'Connection: Keep-Alive'}++;
        # no delay on keep-alives
        return;
    }

    $server->{counts}{'Connection: Close'}++;

    # return if no delay or first request
    return if !$server->{delay_sec} || !$server->{last_response_time};

    my $wait = $server->{delay_sec} - (time - $server->{last_response_time});

    return unless $wait > 0;

    print STDERR "sleeping $wait seconds\n" if $server->{debug} & DEBUG_URL;
    sleep($wait);
}

#=======================================================================
# process_link()  - process a link from the list
#
# Can be called recursively (for auth and redirects)
#
# This does most of the work.
# Pass in:
#   $server -- config hash, plus ugly scratch pad memory
#   $uri    -- uri to fetch and extract links from
#   $parent -- parent uri for better messages
#   $depth  -- for controlling how deep to go into a site, whatever that means
#
# Returns:
#   undef or an array ref of links to add to the list
#
# Makes request, tests response, logs, parsers and extracts links
# Very ugly as this is some of the oldest code
#
#-----------------------------------------------------------------------
sub process_link
{
    my ($server, $uri, $parent, $depth) = @_;

    die "$0: Time Limit Exceeded\n"
        if $server->{max_time} && $server->{max_time} < time;

    # Make request object for this URI
    my $request = HTTP::Request->new('GET', $uri);

    ## HTTP::Message uses Compress::Zlib, and Gisle responded Jan 8, 07 that it's safe to test
    my @encodings;
    eval { require Compress::Zlib };
    push @encodings, qw/gzip x-gzip deflate/ unless $@;

    eval { require Compress::Bzip2 };
    push @encodings, 'x-bzip2' unless $@;

    $request->header('Accept-encoding', join ', ', @encodings) if @encodings;
    $request->header('Referer', $parent) if $parent;

    # Set basic auth if defined - use URI specific first, then credentials
    # this doesn't track what should have authorization
    my $last_auth;
    if ($server->{last_auth})
    {
        my $path = $uri->path;
        $path =~ s!/[^/]*$!!;
        $last_auth = $server->{last_auth}{auth} if $server->{last_auth}{path} eq $path;
    }

    if (my ($user, $pass) = split /:/, ($last_auth || $uri->userinfo || $server->{credentials} || ''))
    {
        $request->authorization_basic($user, $pass);
    }

    my $response;

    delete $server->{response_checked};  # to keep from checking more than once

    if ($server->{use_head_requests})
    {
        $request->method('HEAD');

        # This is ugly in what it can return.  It's can be recursive.
        $response = make_request($request, $server, $uri, $parent, $depth);

        return $response if !$response || ref $response eq 'ARRAY';  # returns undef or an array ref if done

        # otherwise, we have a response object.
        $request->method('GET');
    }

    my $t0 = [gettimeofday];
    # Now make GET request
    $response = make_request($request, $server, $uri, $parent, $depth);

    my $elapsed = tv_interval($t0);
    my $redirect_url;

    # Deal with failed responses - non 2xx
    if (!$response->is_success)
    {
        # Are we rejected because of robots.txt?
        if ($response->status_line =~ 'robots.txt')
        {
            $server->{counts}{'robots.txt'}++;
        }

        # Look for redirect
        elsif ($response->is_redirect)
        {
            $redirect_url = redirect_response($response, $server, $uri, $parent, $depth);
        }

        # Report bad links (excluding those skipped by robots.txt)
        # Not so sure about this being here for these links...
        elsif ($server->{validate_links})
        {
            just_log("val");
            validate_link($server, $uri, $parent, $response);
        }
    }

    # Check for meta refresh
    # requires that $ua->parse_head() is enabled (the default)
    if ($response->header('refresh') && $response->header('refresh') =~ /URL\s*=\s*(.+)/)
    {
        just_log("meta refresh");
        redirect_response($response, $server, $uri, $parent, $depth, $1, 'meta refresh');
    }

    # Check for meta robots tag
    # -- should probably be done in request sub to avoid fetching docs that are not needed
    # -- also, this will not not work with compression $$$ check this
    unless ($server->{ignore_robots_file}  || $server->{ignore_robots_headers})
    {
        if (my $directives = $response->header('X-Meta-ROBOTS'))
        {
            my %settings = map { lc $_, 1 } split /\s*,\s*/, $directives;
        }
    }


if (!$response || ref $response eq 'ARRAY')
{
    just_log("getting out");
}

    return $response if !$response || ref $response eq 'ARRAY';  # returns undef or an array ref

    my $status = ($response->status_line || $response->status || 'unknown status');
    my $content = $response->decoded_content;
    my $bytecount = length $content;

    $server->{counts}{'Total Bytes'} += $bytecount;
    $server->{counts}{'Total Docs'}++;

    # make sure content is unique
    if ($server->{use_md5} && $response->status_line =~ m/200 OK/i)
    {
        my $digest =  $response->header('Content-MD5') || Digest::MD5::md5($response->content);
        if ($visited{ $digest } && $uri ne $visited{ $digest })
        {
            log_response($status . ' Duplicate', $bytecount, $elapsed, $parent, $uri, $depth, $visited{ $digest });

            $server->{counts}{Skipped}++;
            $server->{counts}{'MD5 Duplicates'}++;

            die "$0: Max indexed files Reached\n"
                if $server->{max_indexed} && $server->{counts}{'Total Docs'} >= $server->{max_indexed};

            return;
        }
        $visited{ $digest } = $uri;
    }

    log_response($status, $bytecount, $elapsed, $parent, $uri, $depth, '');

    return $redirect_url if ($redirect_url);

    die " Max indexed files Reached\n"
        if $server->{max_indexed} && $server->{counts}{'Total Docs'} >= $server->{max_indexed};

    return unless ($content);

    # Extract out links (if not too deep)
    my $links_extracted = extract_links($server, \$content, $response)
        unless defined $server->{max_depth} && $depth >= $server->{max_depth};

    return $links_extracted;
}

#=======================================================================
# make_request
#
# This only can deal with things that happen in a HEAD request.
#
# Function to make either a HEAD or GET request
# Returns one of three things:
#   undef - stop processing and return
#   and array ref - a list of URLs extracted (via recursive call)
#   a HTTP::Response object
#
# Yes it's a mess -- got pulled out of other code when adding HEAD requests
#-----------------------------------------------------------------------
sub make_request
{
    my ($request, $server, $uri, $parent, $depth) = @_;

    my $response;
    my $response_aborted_msg;
    my $killed_connection;

    my $ua = $server->{ua};

    if ($request->method eq 'GET')
    {
        # When making a GET request this gets called for every chunk returned
        # from the webserver (well, from the OS).  No idea how bit it will be.
        my $total_length = 0;

        my $callback = sub
        {
            my ($content, $response) = @_;

            # First time, check response
            check_response($response, $server, $uri)
                unless $server->{response_checked}++;

            # In case didn't return a content-length header
            $total_length += length $content;
            check_too_big($response, $server, $total_length) if $server->{max_size};

            $response->add_content($content);
        };

        ## Make Request ##

        # Used to wrap in an eval and use alarm on non-win32 to fix broken $ua->timeout
        $response = $ua->simple_request($request, $callback, 4096);

        # Check for callback death:
        # If the LWP callback aborts
        if ($response->header('client-aborted'))
        {
            $response_aborted_msg = $response->header('X-Died') || 'unknown';
            $killed_connection++;  # so we will delay
        }
    }
    else
    {
        # Make a HEAD request
        $response = $ua->simple_request($request);

        # check_response
        check_response($response, $server, $uri)
            unless $server->{response_checked}++;

        $response_aborted_msg = $@ if $@;
    }

    # Save the request completion time for delay between requests
    $server->{last_response_time} = time;

    # Ok, did the request abort for some reason? (response checker called die())
    if ($response_aborted_msg)
    {
        $server->{counts}{Skipped}++;

        # Not really sure why request aborted. So just print the error
        print STDERR "\n\nRequest for '$uri' aborted because: '$response_aborted_msg'\n";

        # Might be smart to flag to abort but wait until the next chunk
        # to really abort. That might make so the connection would not get killed.

        delete $server->{keep_alive_connection} if $killed_connection;
        return;
    }

    # Look for connection.  Assume it's a keep-alive unless we get a Connection: close
    # header.  Some server errors (on Apache) will close the connection, but they
    # report it.
    # Have to assume the connection is open (without asking LWP) since the first
    # connection we normally do not see (robots.txt) and then following keep-alive
    # connections do not have Connection: header.

    my $connection = $response->header('Connection') || 'Keep-alive';  # assume keep-alive
    $server->{keep_alive_connection} =  !$killed_connection && $server->{keep_alive} && $connection !~ /close/i;

    # Clean up the URI so passwords don't leak
    $response->request->uri->userinfo(undef) if $response->request;
    $uri->userinfo(undef);

    return $response;
}

#=======================================================================
# check_response -- after resonse comes back from server
#-----------------------------------------------------------------------
sub check_response
{
    my ($response, $server, $uri) = @_;

    return unless $response->is_success;  # 2xx response.

    # Cache user/pass if entered from the keyboard or callback function (as indicated by the realm)
    # do here so we know it is correct
    if ($server->{cur_realm} && $uri->userinfo)
    {
        my $key = $uri->canonical->host_port . ':' . $server->{cur_realm};
        $server->{auth_cache}{$key} =  $uri->userinfo;

        # not too sure of the best logic here
        my $path = $uri->path;
        $path =~ s!/[^/]*$!!;
        $server->{last_auth} = { path => $path, auth => $uri->userinfo, };
    }

    # check for document too big.
    check_too_big($response, $server) if $server->{max_size};
}

#=======================================================================
# check_too_big -- see if document is too big
# Die if it is too big.
#-----------------------------------------------------------------------
sub check_too_big
{
    my ($response, $server, $length) = @_;

    $length ||= $response->content_length || 0;
    return unless $length && $length =~ /^\d+$/;

    die "Document exceeded $server->{max_size} bytes (Content-Length: $length) Method: " . $response->request->method . "\n"
        if $length > $server->{max_size};
}

#=======================================================================
# redirect_response -- deal with a 3xx redirect
#
# Returns link to follow
#
#-----------------------------------------------------------------------
sub redirect_response
{
    my ($response, $server, $uri, $parent, $depth, $location, $description) = @_;

    $location ||= $response->header('location');
    unless ($location)
    {
        print STDERR "Warning: $uri returned a redirect without a Location: header\n";
        return;
    }

    $description ||= 'Location';

    # This should NOT be needed, but some servers are broken
    # and don't return absolute links.
    # and this may even break things
    my $u = URI->new_abs($location, $response->base);

    if ($u->canonical eq $uri->canonical)
    {
        print STDERR "Warning: $uri redirects to itself!.\n";
        return;
    }

    # make sure it's ok:
    #return unless check_link($u, $server, $response->base, '(redirect)', $description);

    # make recursive request
    # This will not happen because the check_link records that the link has been seen.
    # But leave here just in case
    if ($server->{_request}{redirects}++ > MAX_REDIRECTS)
    {
        warn "Exceeded redirect limit: perhaps a redirect loop: $uri on parent page: $parent\n";
        return;
    }

    $server->{counts}{"$description Redirects"}++;
    #my $links = process_link($server, $u, $parent, $depth);
    my @links;
    push @links, $u;
    $server->{_request}{redirects}-- if  $server->{_request}{redirects};

    return $u;
}

#=======================================================================
#  Extract links from a text/html page
#
#   Call with:
#       $server - server object
#       $content - ref to content
#       $response - response object
#
#-----------------------------------------------------------------------
sub extract_links
{
    my ($server, $content, $response) = @_;

    return unless $response->header('content-type') &&
                     $response->header('content-type') =~ m[^text/html];

    $server->{Spidered}++;

    my @links;

    my $base = $response->base;
    $visited{ $base }++;  # $$$ come back and fix this (see 4/20/03 lwp post)

    my $p = HTML::LinkExtorRel->new;
    #my $p = HTML::RelExtor->new();
    $p->parse($$content);

    my %skipped_tags;
    my $nofollow;

    for ($p->links)
    {
        my ($tag, $nofollow, %attr) = @$_;

        # which tags to use (not reported in debug)
        my $attr = join ' ', map { qq[$_="$attr{$_}"] } keys %attr;

        print STDERR "Looking at extracted tag '<$tag $attr>'\n" if $server->{debug} & DEBUG_LINKS;

        unless ($nofollow || $server->{ignore_nofollow})
        {
            print STDERR "   nofollow tag skipped\n" if $server->{debug} & DEBUG_LINKS && !$skipped_tags{$tag}++;
            next;
        }

        unless ($server->{link_tags_lookup}{$tag})
        {
            # each tag is reported only once per page
            print STDERR
                "   <$tag> skipped because not one of (",
                join(',', @{$server->{link_tags}}),
                ")\n" if $server->{debug} & DEBUG_LINKS && !$skipped_tags{$tag}++;

            if ($server->{validate_links} && $tag eq 'img' && $attr{src})
            {
                my $img = URI->new_abs($attr{src}, $base);
                validate_link($server, $img, $base);
            }

            next;
        }

        # Grab which attribute(s) which might contain links for this tag
        my $links = $HTML::Tagset::linkElements{$tag};
        $links = [$links] unless ref $links;

        my $found;

        # Now, check each attribute to see if a link exists
        for my $attribute (@$links)
        {
            #print STDERR "ATTTT-$attribute";
            if ($attr{ $attribute }) # ok tag
            {
                # Create a URI object
                my $u = URI->new_abs($attr{$attribute},$base);

                next unless check_link($u, $server, $base, $tag, $attribute);

                push @links, $u;
                print STDERR qq[   $attribute="$u" Added to list of links to follow\n] if $server->{debug} & DEBUG_LINKS;
                $found++;
            }
        }

        print STDERR "  tag did not include any links to follow or is a duplicate\n" if (!$found && $server->{debug} & DEBUG_LINKS);
    }

    print STDERR "! Found ", scalar @links, " links in ", $response->base, "\n" if $server->{debug} & DEBUG_LINKS;

    return \@links;
}

#=======================================================================
# This function check's if a link should be added to the list to spider
#
#   Pass:
#       $u - URI object
#       $server - the server hash
#       $base - the base or parent of the link
#
#   Returns true if a valid link
#
#   Calls the user function "test_url".  Link rewriting before spider
#   can be done here.
#
#-----------------------------------------------------------------------
sub check_link
{
    my ($u, $server, $base, $tag, $attribute) = @_;

    $tag ||= '';
    $attribute ||= '';

    # Kill the fragment
    $u->fragment(undef);

    # Here we make sure we are looking at a link pointing to the correct (or equivalent) host
    unless ($server->{scheme} eq $u->scheme && $server->{same_host_lookup}{$u->canonical->authority||''})
    {
        print STDERR qq[ ?? <$tag $attribute="$u"> skipped because different host\n] if $server->{debug} & DEBUG_LINKS;
        $server->{counts}{'Off-site links'}++;
        validate_link($server, $u, $base) if $server->{validate_links};
        return;
    }

    #$u->host_port($server->{authority});  # Force all the same host name

    # Don't add the link if already seen  - these are so common that we don't report
    # Might be better to do something like $visited{ $u->path } or $visited{$u->host_port}{$u->path};
    if ($visited{ $u->canonical }++)
    {
        #$server->{counts}{Skipped}++;
        $server->{counts}{Duplicates}++;

        # Just so it's reported for all pages
        if ($server->{validate_links} && $validated{$u->canonical})
        {
            push @{$bad_links{ $base->canonical }}, $u->canonical;
        }

        return;
    }

    return 1;
}

#=======================================================================
# This function is used to validate links that are off-site.
#
#   It's just a very basic link check routine that lets you validate the
#   off-site links at the same time as indexing.  Just because we can.
#
#-----------------------------------------------------------------------
sub validate_link
{
    my ($server, $uri, $base, $response) = @_;

    $base = URI->new($base) unless ref $base;
    $uri = URI->new_abs($uri, $base) unless ref $uri;

    # Already checked?
    if (exists $validated{ $uri->canonical })
    {
        # Add it to the list of bad links on that page if it's a bad link.
        push @{$bad_links{ $base->canonical }}, $uri->canonical
            if $validated{ $uri->canonical };

        return;
    }

    $validated{ $uri->canonical } = 0;  # mark as checked and ok.

    unless ($response)
    {
        my $ua = LWP::UserAgent->new(timeout =>  $server->{max_wait_time});
        my $request = HTTP::Request->new('HEAD', $uri->canonical);
        $response = $ua->simple_request($request);
    }

    return if $response->is_success;

    my $error = $response->status_line || $response->status || 'unknown status';

    $error .= ' ' . URI->new_abs($response->header('location'), $response->base)->canonical
        if $response->is_redirect && $response->header('location');

    $validated{ $uri->canonical } = $error;
    push @{$bad_links{ $base->canonical }}, $uri->canonical;
}

#=======================================================================
# Log a response
#-----------------------------------------------------------------------
sub log_response
{
    my ($status, $bytecount, $elapsed, $parent, $uri, $depth, $dupe) = @_;

    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
    my $timestamp = sprintf "%4d-%02d-%02d %02d:%02d:%02d", $year+1900,$mon+1,$mday,$hour,$min,$sec;

    $bytecount = convert($bytecount);

    open(DAT,">>$results_file") || die("Error: Cannot open file $results_file");

    if ($dupe)
    {
        printf DAT ("%s - %-27s %6s %6.3fs - %s => %s %s\n", $timestamp, $status, $bytecount, $elapsed, $parent, $uri, " <= dupe of => $dupe");
    }
    else
    {
        printf DAT ("%s - %-27s %6s %6.3fs - %s => %s\n", $timestamp, $status, $bytecount, $elapsed, $parent, $uri);
    }

    close(DAT);

    if ($use_csv)
    {
        open(DAT,">>$csv_file") || die("Error: Cannot open file $csv_file");
        printf DAT ("%s,%s,%s,%s,%s,%s,%s\n", $timestamp, $status, $bytecount, $elapsed, $parent, $uri, $dupe);
        close(DAT);
    }

    local $| = 1;
    #printf STDERR ("%104s\r Spidering... %.90s\r", " ", $uri);
    back_and_print( "Spidering... $uri" );
}

sub just_log
{
    my ($msg) = @_;

    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
    my $timestamp = sprintf "%4d-%02d-%02d %02d:%02d:%02d", $year+1900,$mon+1,$mday,$hour,$min,$sec;

    printf("%s - %s\n", $timestamp, $msg);
}

sub commify
{
    local $_  = shift;
    1 while s/^([-+]?\d+)(\d{3})/$1,$2/;
    return $_;
}

sub convert
{
    my $size = shift;
    my @args = qw/b K M G/;

    while (@args && $size > 1024)
    {
        shift @args;
        $size /= 1024;
    }

    $size = sprintf("%.1f",$size);

    return "$size$args[0]";
}

#my $Backup_Count = 0;
sub back_and_print
{
    my $text = shift @_;  # no tabs, no newlines!

    #print STDERR "\b" x $Backup_Count, " " x $Backup_Count, "\b" x $Backup_Count;
    #$Backup_Count = length $text;

    if (length $text > 79)
    {
        $text =~ s/(.{26}).*(.{50,})/$1...$2/;
    }
    print STDERR "\r", " " x 79, "\r";
    #print STDERR "\e[1K\r";
    print STDERR $text;

    #http://www.perlmonks.org/?node_id=699555
    #\e[1K - clear from cursor to beginning of the line
    #\r    - goto beginning of the line
    #\e[1J - clear from cursor to beginning of the screen.
    #\e[H  - Moves the cursor to top left corner
}

sub print_usage
{
    print "Usage: perl $0 [OPTION]\n\n";
    print "Optinal:\n";
    print "  -h,  --help               print this help\n";
    print "  -csv,  --csv=FILE         output to CSV file\n";
    print "\n";
}
