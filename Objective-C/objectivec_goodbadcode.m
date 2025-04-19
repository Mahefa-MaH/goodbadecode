// Good Code: Using Blocks for Asynchronous Operations

#import <Foundation/Foundation.h>

@interface MyDataFetcher : NSObject

- (void)fetchDataWithCompletion:(void (^)(NSData *data, NSError *error))completion;

@end

@implementation MyDataFetcher

- (void)fetchDataWithCompletion:(void (^)(NSData *data, NSError *error))completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate fetching data
        sleep(2); 
        NSData *data = [@"This is some fetched data" dataUsingEncoding:NSUTF8StringEncoding];
        
        dispatch_async(dispatch_get_main_queue(), ^{
            completion(data, nil);
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
        [[NSRunLoop currentRunLoop] run];
    }
    return 0;
}


// Bad Code:  Ignoring Error Handling and Blocking the Main Thread

#import <Foundation/Foundation.h>

@interface MyBadDataFetcher : NSObject
- (NSData*) fetchData;
@end

@implementation MyBadDataFetcher
- (NSData*) fetchData{
    sleep(2);
    return [@"This is some fetched data" dataUsingEncoding:NSUTF8StringEncoding];
}
@end


int main2(int argc, const char * argv[]) {
    @autoreleasepool {
        MyBadDataFetcher *badFetcher = [[MyBadDataFetcher alloc] init];
        NSData *data = [badFetcher fetchData]; // Blocks main thread
        NSString *string = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSLog(@"Fetched data (bad): %@", string);
    }
    return 0;
}
