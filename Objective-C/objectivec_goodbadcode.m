// Good Code: Using blocks for asynchronous operations and proper error handling

#import <Foundation/Foundation.h>

@interface MyDataFetcher : NSObject

- (void)fetchDataWithCompletion:(void (^)(NSData *data, NSError *error))completion;

@end

@implementation MyDataFetcher

- (void)fetchDataWithCompletion:(void (^)(NSData *data, NSError *error))completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate fetching data (replace with actual network request)
        sleep(2);  
        NSData *data = [@"Sample Data" dataUsingEncoding:NSUTF8StringEncoding];
        NSError *error = nil;
        
        dispatch_async(dispatch_get_main_queue(), ^{
            completion(data, error);
        });
    });
}

@end


int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MyDataFetcher *fetcher = [[MyDataFetcher alloc] init];
        [fetcher fetchDataWithCompletion:^(NSData *data, NSError *error) {
            if (error) {
                NSLog(@"Error fetching data: %@", error);
            } else {
                NSString *string = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
                NSLog(@"Fetched data: %@", string);
            }
        }];
        [[NSRunLoop currentRunLoop] run]; //Keep the main thread alive until the asynchronous operation is complete.
    }
    return 0;
}


// Bad Code: Ignoring errors, improper memory management, and blocking the main thread

#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        //Simulate a long running task that blocks the main thread.
        sleep(5);
        NSLog(@"This blocks the main thread!");
        // Memory leak: no release of the string.
        NSString* str = [NSString stringWithFormat:@"This is a string"];
        NSLog(@"%@", str);
        return 0;
    }
}
