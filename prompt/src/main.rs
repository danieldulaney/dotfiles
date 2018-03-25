extern crate ansi_term;
extern crate git2;
extern crate users;
extern crate hostname;

use ansi_term::{Color::*, Style};
use git2::Repository;
use users::{get_effective_username, get_effective_uid};
use hostname::get_hostname;

use std::env;

static REPLACEMENT: &'static str = "???";

fn working_dir() -> Option<String> {
    match env::current_dir() {
        Err(_) => None,
        Ok(p) => match p.into_os_string().into_string() {
            Ok(s) => Some(s),
            Err(_) => None,
        }
    }
}

fn home_dir() -> Option<String> {
    match env::home_dir() {
        None => None,
        Some(p) => match p.into_os_string().into_string() {
            Ok(s) => Some(s),
            Err(_) => None,
        }
    }
}

fn clean_working_dir() -> Option<String> {
    let cwd = match working_dir() {
        Some(s) => s,
        None => return None,
    };

    let home = match home_dir() {
        Some(s) => s,
        None => return Some(cwd),
    };

    if cwd.starts_with(&home) {
        let mut s = String::from("~");
        s.push_str(&cwd[home.len()..]);

        Some(s)
    } else {
        Some(cwd)
    }
}

fn location() -> String {
    match clean_working_dir() {
        Some(s) => s,
        None => String::from(REPLACEMENT),
    }
}

fn is_root() -> bool {
    match get_effective_uid() {
        0 => true,
        _ => false,
    }
}

fn git_repo() -> Option<Repository> {
    match Repository::open_from_env() {
        Ok(repo) => Some(repo),
        Err(_) => None,
    }
}

fn git_head(repo: &Repository) -> Option<(bool, &'static str, String)> {
    match repo.head() {
        Err(_) => None,
        Ok(h) => match repo.head_detached() {
            Ok(false) => h.shorthand().map(str::to_string).map(|s| (true, "on", s)),
            _ => h.target().map(format_oid).map(|s| (false, "at", s)),
        },
    }
}

fn git_activity(repo: &Repository) -> Option<&'static str> {
    use git2::RepositoryState::*;

    match repo.state() {
        Clean => None,
        Merge => Some("merge"),
        Revert => Some("reverting"),
        RevertSequence => Some("reverting sequence"),
        CherryPick => Some("cherry-picking"),
        CherryPickSequence => Some("cherry-picking sequence"),
        Bisect => Some("bisecting"),
        Rebase => Some("rebasing"),
        RebaseInteractive => Some("interactive rebasing"),
        RebaseMerge => Some("merge rebasing"),
        ApplyMailbox => Some("applying from mailbox"),
        ApplyMailboxOrRebase => Some("applying from mailbox"),
    }
}

fn format_oid(oid: git2::Oid) -> String {
    let oid = oid.as_bytes();

    return format!("{:x}{:x}{:x}{:x}", oid[0], oid[1], oid[2], oid[3]);
}

fn hostname() -> String {
    match get_hostname() {
        None => String::from(REPLACEMENT),
        Some(s) => s,
    }
}

fn username() -> String {
    match get_effective_username() {
        None => String::from(REPLACEMENT),
        Some(s) => s,
    }
}

fn prompt() -> &'static str {
    match is_root() {
        true => "#",
        false => "$",
    }
}

fn main() {
    print!("[{}{}{}]",
        match is_root() {
            true => Red,
            false => Cyan,
        }.paint(username()),
        Cyan.paint("@"),
        Cyan.paint(hostname()));

    print!(" {}", Purple.paint(location()));

    if let Some(repo) = git_repo() {
        print!(" (");

        if let Some(activity) = git_activity(&repo) {
            print!("{} ", Red.paint(activity));
        }

        if let Some((named, preposition, head)) = git_head(&repo) {
            print!("{} {}", preposition, match named {
                true => Green,
                false => Blue,
            }.paint(head));
        }

        print!(")");
    }

    println!("");

    println!("{} ", 
        match is_root() {
            true => Style::new().fg(Black).on(Red),
            false => Style::new(),
        }.paint(prompt()));
}

