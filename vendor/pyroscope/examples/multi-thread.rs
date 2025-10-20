extern crate pyroscope;

use pyroscope::{PyroscopeAgent, Result};
use pyroscope_pprofrs::{pprof_backend, PprofConfig};
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    thread,
};

fn hash_rounds1(n: u64) -> u64 {
    let hash_str = "Some string to hash";
    let mut default_hasher = DefaultHasher::new();

    for _ in 0..n {
        for _ in 0..1000 {
            default_hasher.write(hash_str.as_bytes());
        }
        hash_str.hash(&mut default_hasher);
    }

    n
}

fn hash_rounds2(n: u64) -> u64 {
    let hash_str = "Some string to hash";
    let mut default_hasher = DefaultHasher::new();

    for _ in 0..n {
        for _ in 0..1000 {
            default_hasher.write(hash_str.as_bytes());
        }
        hash_str.hash(&mut default_hasher);
    }

    n
}

fn extra_rounds1(n: u64) -> u64 {
    let hash_str = "Some string to hash";
    let mut default_hasher = DefaultHasher::new();

    for _ in 0..n {
        for _ in 0..1000 {
            default_hasher.write(hash_str.as_bytes());
        }
        hash_str.hash(&mut default_hasher);
    }

    n
}

fn extra_rounds2(n: u64) -> u64 {
    let hash_str = "Some string to hash";
    let mut default_hasher = DefaultHasher::new();

    for _ in 0..n {
        for _ in 0..1000 {
            default_hasher.write(hash_str.as_bytes());
        }
        hash_str.hash(&mut default_hasher);
    }

    n
}

fn main() -> Result<()> {
    let agent = PyroscopeAgent::builder("http://localhost:4040", "example.multithread")
        .tags([("Host", "Rust")].to_vec())
        .backend(pprof_backend(PprofConfig::new().sample_rate(100)))
        .build()?;

    // Show start time
    let start = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    println!("Start Time: {}", start);

    // Start Agent
    let agent_running = agent.start()?;

    let (add_tag, remove_tag) = agent_running.tag_wrapper();

    let handle_1 = thread::Builder::new()
        .name("thread-1".to_string())
        .spawn(move || {
            hash_rounds1(300_000);
            add_tag("extra".to_string(), "round-1".to_string()).unwrap();
            extra_rounds1(200_000);
            remove_tag("extra".to_string(), "round-1".to_string()).unwrap();
        })?;

    let (add_tag, remove_tag) = agent_running.tag_wrapper();

    let handle_2 = thread::Builder::new()
        .name("thread-2".to_string())
        .spawn(move || {
            add_tag("extra".to_string(), "round-2".to_string()).unwrap();
            extra_rounds2(100_000);
            remove_tag("extra".to_string(), "round-2".to_string()).unwrap();
            hash_rounds2(500_000);
        })?;

    // Wait for the threads to complete
    handle_1.join().unwrap();
    handle_2.join().unwrap();

    // Stop Agent
    let agent_ready = agent_running.stop()?;

    // Shutdown the Agent
    agent_ready.shutdown();

    // Show program exit time
    let exit = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    println!("Exit Time: {}", exit);

    Ok(())
}
