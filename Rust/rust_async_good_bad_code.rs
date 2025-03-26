use tokio::time::{sleep, Duration};

// Good Code: Using async/await with proper error handling and structured concurrency.
async fn good_code() -> Result<(), Box<dyn std::error::Error>> {
    let handles = (0..5).map(|i| {
        tokio::spawn(async move {
            sleep(Duration::from_millis(i as u64 * 100)).await;
            println!("Task {} finished", i);
            Ok::<_, Box<dyn std::error::Error>>(()) //Proper error handling
        })
    }).collect::<Vec<_>>();

    for handle in handles {
        handle.await??; //Proper await and error handling
    }
    Ok(())
}


// Bad Code:  Ignoring errors, using threads instead of async tasks, and lacks structure.
fn bad_code() {
    for i in 0..5 {
        std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(i as u64 * 100));
            println!("Task {} finished", i);
            //Ignoring potential errors
        });
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    good_code().await?;
    println!("Good code finished");

    bad_code();
    println!("Bad code finished (but error handling is missing!)");
    Ok(())
}

