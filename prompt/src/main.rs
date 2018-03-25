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

fn git_ahead_behind(repo: &Repository) -> Option<(u32, u32)> {
    None
}

// (staged files, unstaged changed, unstaged new)
fn git_status(repo: &Repository) -> (u32, u32, u32) {

    use git2::Status;

    let mut staged_count: u32 = 0;
    let mut changed_count: u32 = 0;
    let mut untracked_count: u32 = 0;

    let mut options = git2::StatusOptions::new();
    options.include_untracked(true);
    options.recurse_untracked_dirs(true);
    options.recurse_ignored_dirs(false);

    let mut staged = Status::empty();
    staged.insert(Status::INDEX_NEW);
    staged.insert(Status::INDEX_MODIFIED);
    staged.insert(Status::INDEX_DELETED);
    staged.insert(Status::INDEX_RENAMED);
    staged.insert(Status::INDEX_TYPECHANGE);

    let mut changed = Status::empty();
    changed.insert(Status::WT_MODIFIED);
    changed.insert(Status::WT_DELETED);
    changed.insert(Status::WT_RENAMED);
    changed.insert(Status::WT_TYPECHANGE);

    let untracked = Status::WT_NEW;

    for entry in repo.statuses(Some(&mut options)).unwrap().iter() {
        let status = entry.status();

        if staged.contains(status) {
            staged_count += 1;
        } else if changed.contains(status) {
            changed_count += 1;
        } else if untracked.contains(status) {
            untracked_count += 1;
        }
    }

    return (staged_count, changed_count, untracked_count);
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
            print!("{}", Red.paint(activity));
        }

        if let Some((named, preposition, head)) = git_head(&repo) {
            print!("{} {}", Blue.paint(preposition), Blue.paint(head));
        }

        let (staged, changed, untracked) = git_status(&repo);

        if staged > 0 {
            print!(", {}", Green.paint(format!("{} staged", staged)));
        }

        if changed > 0 {
            print!(", {}", Yellow.paint(format!("{} changed", changed)));
        }

        if untracked > 0 {
            print!(", {}", Red.paint(format!("{} untracked", untracked)));
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

