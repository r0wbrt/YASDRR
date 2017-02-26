--Copyright Robert C. Taylor, All Rights Reserved

{- |
Module      :  YASDRR.Threading.Sharding
Description :  Library to create a single producer to many worker to single consumer.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

This library takes a single producer thread and routes its output to be 
processed by a group of worker threads. These worker threads then route their 
output to a single consumer thread.
-}

module YASDRR.Threading.Sharding (shardResource, 
                                      shardResourceComplicated,
                                          defaultNumberOfThreads,
                                              defaultQueueSize,
                                                  waitForCompletion,
                                                      isComplete,
                                                          ShardProducer,
                                                              ShardConsumer,
                                                                  ShardWorker) 
    where

import qualified Control.Concurrent.Chan.Unagi.Bounded as CCCUB
import Control.Concurrent.MVar
import Control.Concurrent
-- import Data.Sequence as Seq


-- | Format of message passed between sharding threads
data MessageType a = MessageDie | MessageData Int a 
                   | MessageNewThread | MessageThreadDied
                   
type ShardProducer state output = state -> IO (Maybe (output, state))
type ShardConsumer state input = state -> input -> IO (Maybe state)
type ShardWorker state input output = state -> input 
    -> IO (Maybe (output, state))

-- | Begins the sharding of a shared resource using pre-defined settings. 
--   Acts like a wrapper around shardResourceComplicated. Applications should 
--   opt into using this over shardResourceComplicated.
shardResource :: ShardProducer stateProd input -> stateProd -> 
                    ShardConsumer stateCons output -> stateCons ->
                        ShardWorker stateWorker input output -> stateWorker -> 
                           IO [MVar ()]
shardResource shardProducer prodState shardConsumer consState shardWorker workState = do
    
    -- Grab default settings
    queueSize <- defaultQueueSize
    threadCount <- defaultNumberOfThreads
    
    --Pass these settings on to the core sharding function.
    shardResourceComplicated shardProducer prodState shardConsumer consState shardWorker workState threadCount queueSize

    
-- | Main entry function to trigger the sharding of a shared resource. This 
--   function allows applications full control over all internal
--   parameters which are exposed as parameters on this function.
shardResourceComplicated :: ShardProducer stateProd input -> stateProd -> 
                                ShardConsumer stateCons output -> stateCons ->
                                ShardWorker stateWorker input output -> 
                                stateWorker -> Int -> Int -> IO [MVar ()]
shardResourceComplicated shardProducer prodState shardConsumer consState shardWorker workState numberOfThreads queueSize = do
    
    --MVars that represent if the produce and consumer threads are still active.
    producerThreadMvar <- newEmptyMVar
    consumerThreadMvar <- newEmptyMVar
    
    --Set up the channel what will be used to comunicate between the producer,
    --consumer, and worker threads.
    (inToWorkers, outToWorkers) <- CCCUB.newChan queueSize
    (inFromWorkers, outFromWorkers) <- CCCUB.newChan queueSize
    
    --Create the consumer and producer thread
    _ <- forkFinally (shardProducerThread shardProducer inToWorkers numberOfThreads 0 prodState) (threadDeathHandler producerThreadMvar "producer")
    
    _ <- forkFinally (shardConsumerThread shardConsumer outFromWorkers numberOfThreads [] 0 consState) (threadDeathHandler consumerThreadMvar "Consumer")
    
    --Create the worker threads that will process the sharded data.
    workerThreadList <- createWorkerThreads numberOfThreads shardWorker workState outToWorkers inFromWorkers
    
    --Return a list of mvars representing the status of these threads.
    return ([producerThreadMvar,consumerThreadMvar] ++ workerThreadList)
  
  
-- | Creates a set up worker threads. 
createWorkerThreads :: Int -> ShardWorker state input output -> state -> 
                                CCCUB.OutChan (MessageType input) -> 
                                CCCUB.InChan (MessageType output) -> IO [MVar ()]
createWorkerThreads numOfThreads shardWorker workState outToWorkers inFromWorkers = spawnThreads numOfThreads []

    -- Internal loop to generate the threads.
    -- When count is zero, return the list of thread Mvars
    where spawnThreads 0 acc = return acc
          
          --When count is not zero, spawn a thread, deincrement the counter, 
          --and recurse.
          spawnThreads count acc = do
            threadMvar <- newEmptyMVar
            _ <- forkFinally (shardWorkerThread shardWorker workState outToWorkers inFromWorkers) (threadDeathHandler threadMvar "Worker")
            spawnThreads (count - 1) (threadMvar:acc)

threadDeathHandler :: Show b => MVar () -> String -> Either b a -> IO ()
threadDeathHandler threadMvar threadName event = do
    putMVar threadMvar ()
    case event of
         Left exception -> error $ "Error " ++ threadName ++ " Thread died with error: " ++ show exception
         Right _ -> return ()
            
-- | The number of worker thereads spawned defaults to the number
--   of of capabilities available. 
defaultNumberOfThreads :: IO Int
defaultNumberOfThreads = getNumCapabilities


-- | The default queue size is the number of threads times 16. 
defaultQueueSize :: IO Int
defaultQueueSize = do
    n <- defaultNumberOfThreads
    return (n * 512)

    
-- | This function blocks until all threads have quit.
waitForCompletion :: [MVar a] -> IO ()
waitForCompletion [] = return ()
waitForCompletion (x:xs) = do
    _ <- takeMVar x
    waitForCompletion xs

    
-- | This function will determine if any threads are still active 
--   in a non-blocking manner.
isComplete :: [MVar a] -> IO Bool
isComplete [] = return True

isComplete (x:xs) = do
    
    value <- isEmptyMVar x
    
    if value then
        return False
    else 
        isComplete xs


-- | Thread that manages calling the producer function.
--   Used internally by the sharding framework.
shardProducerThread :: ShardProducer state input -> 
                            CCCUB.InChan (MessageType input)  -> Int -> 
                            Int -> state -> IO ()
shardProducerThread producer outChannel threadCount tagPos state = do
    
    --Get a single result from the producer function.
    result <- producer state
    
    --Producer function will return a result if more data is to 
    -- be processed or nothing.
    case result of
         
         --More data to be processed
        Just (msgData, newState) -> do
            
            --Make sure to be strict since we want the data to be fully 
            --evaulated in the producer thread.
            --Note this call will block if the outChannel is full.
            msgData `seq` CCCUB.writeChan outChannel (MessageData tagPos msgData)
            
            --Recurse to get the next block of output.
            shardProducerThread producer outChannel threadCount (tagPos + 1) newState
            
        --No more data to process. Shutdown the sharding.
        Nothing -> handleShutdown threadCount
        
        --Helper looping function to aid in the shutdown process.
        --Will send N number of Die messages where N is equal to the number
        --of threads in existence. This should not leave any channels full since
        --the channels are expected to preserve the order of the sent messages 
        --and the worker thread should not take any additional messages out 
        --of the channel after it receives MessageDie.
    where   handleShutdown 0 = return ()
            handleShutdown count = do
                CCCUB.writeChan outChannel MessageDie
                handleShutdown (count - 1)
                
                
-- | This thread manages the shard reducing function which takes the output 
--   from all of the worker threads and reduces it to its final result.
shardConsumerThread :: ShardConsumer state input -> 
                        CCCUB.OutChan (MessageType input) -> Int -> 
                        [(Int, input)] -> Int -> state -> IO ()
shardConsumerThread _ _ 0 _ _ _ = return ()

shardConsumerThread consumer inChannel threadCount messageList tagPos state = do
    
    --Get the next message to process with the expectation that the order of 
    --recieved messages may be out of order or it may be a control message.
    message <- CCCUB.readChan inChannel
    case message of
         
         --Indicates that the consumer thread should quit.
         MessageDie -> return ()
         
         --Inidcates there is a new worker thread.
         MessageNewThread -> shardConsumerThread consumer inChannel (threadCount + 1) messageList tagPos state
         
         --A worker thread has died. 
         MessageThreadDied -> shardConsumerThread consumer inChannel (threadCount - 1) messageList tagPos state
         
         --A new data message to process.
         MessageData tag msgData -> do
             
             --Handle the next received message.
             result <- handleMessage consumer state messageList tagPos tag msgData
             
             --Handle message returns nothing if there is a problem. 
             --If no problem has occured, recurse.
             case result of
                Nothing -> return ()
                Just (newTagPos, newState, newMessageList) -> shardConsumerThread consumer inChannel threadCount newMessageList newTagPos newState

                
-- | Handles receiving a message, checks to see if this message is the one we
--   want next, and if not, stored it for later use.
handleMessage :: ShardConsumer state input -> state -> [(Int, input)] -> 
                    Int -> Int -> input 
                        -> IO (Maybe (Int, state, [(Int, input)]))
handleMessage consumer state messageList tagPos tag msgData =
    
    --Is this the next message we expect?
    if tag==tagPos then
    
        do
            --The consumer function returns nothing if there is a problem
            --Otherwise it returns a state variable that will be passed to it
            --in the next call.
            result <- consumer state msgData
            
            case result of
                 
                        --Error state, return nothing to the caller to indicate
                        --a problem occured
                        Nothing -> return Nothing
                        
                        --Check to see if there are more messages in the queue
                        --that can be processed
                        Just newState -> handleResiduleMessage consumer newState messageList (tagPos + 1)
    else
        --Return the new internal state of handleMessage
        return $ Just (tagPos, state, addToQueue messageList tag msgData)
                          

-- | Process any messages that are remaining on the queue.
handleResiduleMessage :: ShardConsumer state input -> state
                            -> [(Int, input)] -> Int -> 
                            IO (Maybe (Int, state, [(Int, input)]))
handleResiduleMessage _ state [] tagPos = return $ Just (tagPos, state, [])

handleResiduleMessage consumer state messageList tagPos = 
    
    --Branch based on if the next message in the list is the next one we want.
    if tag == tagPos then
        do
            --Call our handler function, and if nothing goes wrong, handle 
            --the next message in our queue.
            result <- consumer state msgData
            
            case result of
                    Nothing ->return Nothing
                    Just newState -> handleResiduleMessage consumer newState (tail messageList) (tagPos + 1)
                    
    --The top message in our queue is not the one we want, return the internal 
    --state for future use
    else
        return $ Just (tagPos, state, messageList)
        
    where (tag, msgData) = head messageList

    
-- | Adds a message to our simple list based priority queue.    
addToQueue :: [(Int, a)] -> Int -> a -> [(Int, a)]
addToQueue [] tag block = [(tag,block)]

addToQueue queue tag block = front ++ ((tag,block):back) 
    
    where (front, back) = break (\(a,_) -> (a > tag)) queue

    
-- | Host for the worker thread function.
shardWorkerThread :: ShardWorker state input output -> state -> 
                        CCCUB.OutChan (MessageType input) ->
                        CCCUB.InChan (MessageType output) ->
                        IO ()
shardWorkerThread worker state inChannel outChannel = do
    
    --Get the next message which may be a control message or a data message.
    msg <- CCCUB.readChan inChannel
    
    --Handle the various types of messages that this thread may receive.
    case msg of
         
         --Quit the thread and let the consumer thread know we are gone.
         MessageDie -> do
             CCCUB.writeChan outChannel MessageThreadDied
             return ()
             
         --New Data to process. Note, we will eventually need to have a more 
         --fault tolerent method of alerting the consumer and producer if the 
         --worker function somehow causes us to crash by raising an exception.
         MessageData tag msgData -> do
             
             --Handle the result of the message.
             result <- worker state msgData
             
             --If the worker function returns nothing, then quit, and notify 
             --the consumer thread that we are dead.
             case result of
                  
                  Nothing -> do
                      CCCUB.writeChan outChannel MessageThreadDied
                      return ()
                      
                  Just (output, newState) -> do
                      CCCUB.writeChan outChannel $ MessageData tag output
                      shardWorkerThread worker newState inChannel outChannel
         
         --Invalid message recieved.
         _ -> do
             CCCUB.writeChan outChannel MessageThreadDied
             error "Unexpected message received by shard worker thread"
                  
 
